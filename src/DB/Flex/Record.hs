{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
  , TypeSynonymInstances, FlexibleInstances, UndecidableInstances
  , OverlappingInstances, TypeFamilies, Rank2Types, KindSignatures
  , TypeOperators, LiberalTypeSynonyms, FlexibleContexts, ScopedTypeVariables
  , GADTs, TemplateHaskell, StandaloneDeriving, DeriveDataTypeable  #-}
module DB.Flex.Record where

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad.Identity

import Data.Char
import Data.Convertible
import qualified Data.HashMap.Strict as Map
import Data.Label
import Data.Label.Util
import Data.Maybe
import Data.Proxy
import Database.HDBC
import Data.UUID
import Data.Time.Clock
import qualified Data.Text
import Data.Typeable

import Data.Record.Abstract
import Data.Foldable1
import Data.Functor1
import Data.Traversable1
import Data.Zippable1

import Language.Haskell.TH
import Language.Haskell.TH.Util

import GHC.Float

import Safe

import Data.Aeson hiding (Number)
import Data.Aeson.Types hiding (Number)
import qualified Data.JSON.Schema as S
import qualified Data.JSON.Schema.Combinators as S
import Text.XML.HXT.Arrow.Pickle hiding (Schema)

data Tup1 f g a = Tup1 (f a) (g a)
data ConstVal a b = ConstVal { unConstVal :: a }

-- * Database fields
data FieldName a = FieldName { unFieldName :: String }

class (Convertible a SqlValue, Convertible SqlValue a,
      XmlPickler a, S.JSONSchema a, ToJSON a, FromJSON a) => DBType a where 
  dbTypeRep :: Proxy a -> String
  nullable :: Proxy a -> Bool
  nullable _ = False

instance DBType a => DBType (Maybe a) where
  dbTypeRep    = dbTypeRep . fmap (fromJust)
  nullable _ = True

instance XmlPickler UTCTime where xpickle = xpPrim
instance XmlPickler Char where xpickle = xpPrim
instance XmlPickler Float where xpickle = xpPrim
instance XmlPickler Double where xpickle = xpPrim
instance XmlPickler Bool where xpickle = xpPrim
instance XmlPickler String where xpickle = xpXmlText
instance XmlPickler Data.Text.Text where xpickle = xpPrim

instance S.JSONSchema Char where schema _ = S.Value (S.LengthBound (Just 1) (Just 1))
instance S.JSONSchema Float where schema _ = S.Number S.unbounded
instance S.JSONSchema Double where schema _ = S.Number S.unbounded
instance S.JSONSchema String where schema _ = S.Value S.unboundedLength

instance XmlPickler UUID where xpickle = xpPrim
instance S.JSONSchema UUID where schema _ = S.Value S.unboundedLength
instance ToJSON UUID where
  toJSON = String . Data.Text.pack . toString
instance FromJSON UUID where
  parseJSON (String s) = maybe mzero return $ fromString $ Data.Text.unpack s
  parseJSON _ = mzero

instance DBType String where dbTypeRep _  = "text"
instance DBType Data.Text.Text where dbTypeRep _  = "text"
instance DBType Char where dbTypeRep _    = "char(1)"
instance DBType Int where dbTypeRep _     = "integer"
instance DBType Integer where dbTypeRep _ = "numeric"
instance DBType Float where dbTypeRep _   = "real"
instance DBType Double where dbTypeRep _  = "double precision"
instance DBType Bool where dbTypeRep _    = "boolean"
instance DBType UUID where dbTypeRep _    = "uuid"
instance DBType UTCTime where dbTypeRep _ = "timestamp with time zone"

instance Convertible UUID SqlValue where
  safeConvert = safeConvert . show

instance Convertible SqlValue UUID where
  safeConvert sql =
    do str <- safeConvert sql
       let uid = readMay str
       maybe (convError "Error converting String to UUID in Data.UUID.Instances." sql) return uid

instance Convertible Float SqlValue where
  safeConvert = safeConvert . float2Double

instance Convertible SqlValue Float where
  safeConvert = fmap double2Float . safeConvert

data Field a where
  Field :: DBType a => Field a

class (Zippable1 r, Traversable1 r) => Record r where
  recordFields :: r Field

class (Record t, AbstractLenses t) => Table t where
  tableName    :: t v -> String
  fieldNames   :: t FieldName

instance DBType a => Record (AbstractVal a) where
  recordFields = AbstractVal Field

niceField :: FieldName a -> String
niceField = dropWhile (not . isAlpha) . unFieldName

buildRecord :: Record r => [SqlValue] -> r Identity
buildRecord vs = distribute (\Field v -> Identity $ fromSql v) vs recordFields

nothingRecord :: Record r => r Maybe
nothingRecord = distribute (\_ _ -> Nothing) (repeat "hoi") recordFields

justRecord :: Record r => r Identity -> r Maybe
justRecord = fmap1 (Just . runIdentity)

recordValues :: Record r => r Identity -> [SqlValue]
recordValues = collect (\(Tup1 Field v) -> toSql $ runIdentity v) . zip1 Tup1 recordFields

names :: forall t v. Table t => t v -> [String]
names _ = foldrf (\v ac -> unFieldName v : ac) [] (fieldNames :: t FieldName)

mkRecordFields :: Name -> Q [Dec]
mkRecordFields = withTyConReify mkRecordFields'

mkRecordFields' :: Dec -> Q [Dec]
mkRecordFields' (DataD _ tnm pars [con] _) =
  let (ps, f) = (init &&& last) $ map getTV pars
      (cnm, tps) = getCons con
      cls = Clause [] (NormalB $ foldl AppE (ConE cnm) $ map mkType tps) []
      mkType (AppT tf a) | tf == VarT f = ConE (mkName "Field")
                         | a == VarT f  = VarE (mkName "recordFields")
      mkType _ = error "Not a valid field"
  in return [InstanceD
                 []
                 (ConT (mkName "Record") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                 [FunD (mkName "recordFields") [cls]]
            ]
mkRecordFields' _ = error $ "mkRecordField only works for datatypes"

dbRecord :: Name -> Q [Dec]
dbRecord = withTyConReify dbRecord'

dbRecord' :: Dec -> Q [Dec]
dbRecord' dc =
  do abst <- mkAbstractType' dc
     func <- mkFunctor1'     (head abst)
     fold <- mkFoldable1'    (head abst)
     trav <- mkTraversable1' (head abst)
     zipp <- mkZippable1'    (head abst)
     flds <- mkRecordFields' (head abst)
     return $ abst ++ func ++ fold ++ trav ++ zipp ++ flds

data PUM m a = PUM { unPUM :: PU (m a) }

instance Table t => XmlPickler (t Maybe) where
  xpickle = xpElem tName $ foldrf foldFunc (xpLift nothingRecord) fieldFuncs
    where tName = tableName (undefined :: t v)

          fieldPickles :: t (PUM Maybe)
          fieldPickles = zip1 mkField recordFields fieldNames
          mkField (Field :: Field a) fn = PUM $ xpOption (xpElem (niceField fn) (xpickle :: PU a))

          fieldFuncs :: t (Const (PU (t Maybe) -> PU (t Maybe)))
          fieldFuncs = zip1 injectField fieldPickles abstractLenses
          injectField (PUM pum) (ALens l) = Const $ \pu ->
            xpWrap (\(a,b) -> set l a b, \b -> (get l b, b)) $ xpPair pum pu

          foldFunc (Const f) acc = f acc

instance Table t => XmlPickler (t Identity) where
  xpickle = xpElem tName $ foldrf foldFunc (xpLift (error "This should not be evaluated Record.hs")) fieldFuncs
    where tName = tableName (undefined :: t v)

          fieldPickles :: t (PUM Identity)
          fieldPickles = zip1 mkField recordFields fieldNames
          mkField (Field :: Field a) fn =
            PUM $ xpWrap (Identity, runIdentity) $ xpElem (niceField fn) (xpickle :: PU a)

          fieldFuncs :: t (Const (PU (t Identity) -> PU (t Identity)))
          fieldFuncs = zip1 injectField fieldPickles abstractLenses
          injectField (PUM pum) (ALens l) = Const $ \pu ->
            xpWrap (\(a,b) -> set l a b, \b -> (get l b, b)) $ xpPair pum pu

          foldFunc (Const f) acc = f acc

instance Table t => S.JSONSchema (t Maybe) where
  schema _ = getConst $ foldrf foldFunc (Const S.empty) fields
    where fields :: t (Const S.Schema)
          fields = zip1 mkField recordFields fieldNames
          mkField (Field :: Field a) fn =
              Const $ S.field (Data.Text.pack (niceField fn)) True (S.schema (undefined :: Proxy a))

          foldFunc (Const f1) (Const f2) = Const $ f1 `S.merge` f2

instance Table t => S.JSONSchema (t Identity) where
  schema _ = getConst $ foldrf foldFunc (Const S.empty) fields
    where fields :: t (Const S.Schema)
          fields = zip1 mkField recordFields fieldNames
          mkField (Field :: Field a) fn =
              Const $ S.field (Data.Text.pack (niceField fn)) False (S.schema (undefined :: Proxy a))

          foldFunc (Const f1) (Const f2) = Const $ f1 `S.merge` f2

instance Table t => ToJSON (t Maybe) where
  toJSON t = Object $ foldrf foldFunc Map.empty fields
    where fieldVals :: t (Const (Maybe Value))
          fieldVals = zip1 mkFieldValue recordFields t
          mkFieldValue (Field :: Field a) = Const . fmap (toJSON :: a -> Value)

          fields :: t (Const (Maybe (Data.Text.Text, Value)))
          fields = zip1 mkField fieldNames fieldVals
          mkField fn = Const . fmap ((,) (Data.Text.pack $ niceField fn)) . getConst

          foldFunc (Const (Just (key, value))) kmap = Map.insert key value kmap
          foldFunc (Const Nothing) kmap = kmap

instance ToJSON a => ToJSON (Identity a) where
  toJSON = toJSON . runIdentity

instance Table t => ToJSON (t Identity) where
  toJSON t = Object $ foldrf foldFunc Map.empty fields
    where fieldVals :: t (Const Value)
          fieldVals = zip1 mkFieldValue recordFields t
          mkFieldValue (Field :: Field a) = Const . (toJSON :: Identity a -> Value)

          fields :: t (Const (Data.Text.Text, Value))
          fields = zip1 mkField fieldNames fieldVals
          mkField fn = Const . ((,) (Data.Text.pack $ niceField fn)) . getConst

          foldFunc (Const (key, value)) kmap = Map.insert key value kmap

data ParserM m a = ParserM { unParserM :: Parser (m a) }

instance Table t => FromJSON (t Maybe) where
  parseJSON (Object items) = traverse1 unParserM fieldParsers
    where fieldParsers :: t (ParserM Maybe)
          fieldParsers = zip1 mkField recordFields fieldNames

          mkField :: forall a.Field a -> FieldName a -> ParserM Maybe a
          mkField (Field :: Field a) fn = ParserM $ ((items .:? (Data.Text.pack $ niceField fn)) :: Parser (Maybe a))

  parseJSON _ = mzero

instance FromJSON a => FromJSON (Identity a) where
  parseJSON = fmap Identity . parseJSON

instance Table t => FromJSON (t Identity) where
  parseJSON (Object items) = traverse1 unParserM fieldParsers
    where fieldParsers :: t (ParserM Identity)
          fieldParsers = zip1 mkField recordFields fieldNames

          mkField :: forall a.Field a -> FieldName a -> ParserM Identity a
          mkField (Field :: Field a) fn = ParserM $ ((items .: (Data.Text.pack $ niceField fn)) :: Parser (Identity a))

  parseJSON _ = mzero

data Visibility = Skip | Required | Optional

data FieldSpec a =
  FieldSpec {
    visibility :: Visibility
  }

class Table t => RecordSpec v t | v -> t where
  recordName :: Proxy v -> String
  recordName _ = tableName (undefined :: t x)

  fieldSpecs :: Proxy v -> t FieldSpec

data FieldData v a =
    NoData
  | GotData a
  deriving Typeable

fd2maybe :: FieldData v a -> Maybe a
fd2maybe NoData = Nothing
fd2maybe (GotData a) = Just a

maybe2fd :: Maybe a -> FieldData v a
maybe2fd Nothing = NoData
maybe2fd (Just a) = GotData a

gotData :: FieldData v a -> a
gotData (GotData a) = a
gotData _ = error "gotData: Found ocnstructor NoData"

nodataRecord :: Record r => r (FieldData v)
nodataRecord = distribute (\_ _ -> NoData) (repeat "hoi") recordFields

allRequired :: Record r => r FieldSpec
allRequired = distribute (\_ _ -> FieldSpec Required) (repeat "hoi") recordFields

applySpec :: forall v t. RecordSpec v t => v -> t Identity -> t (FieldData v)
applySpec _ t = zip1 specField t (fieldSpecs (undefined :: Proxy v))
  where specField _ (FieldSpec Skip) = NoData
        specField v _ = GotData (runIdentity v)

readSpec :: forall v t. RecordSpec v t => v -> t (FieldData v) -> Either String (t (FieldData v))
readSpec _ t = Right t

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight: got a Left"

skip :: FieldSpec a
skip = FieldSpec Skip

opt :: FieldSpec a
opt = FieldSpec Optional

required :: FieldSpec a
required = FieldSpec Required

instance RecordSpec v t => XmlPickler (t (FieldData v)) where
  xpickle = xpElem tName $ foldrf foldFunc (xpLift nodataRecord) fields
    where tName = recordName (undefined :: Proxy v)

          fields :: t (Const (PU (t (FieldData v)) -> PU (t (FieldData v))))
          fields = zip1_4 mkField recordFields fieldNames abstractLenses (fieldSpecs (undefined :: Proxy v))

          mkField :: forall a. Field a -> FieldName a -> (:>:) t a -> FieldSpec a ->
                        Const (PU (t (FieldData v)) -> PU (t (FieldData v))) a
          mkField Field fn (ALens l) spec =
            let pa = case visibility spec of
                        Skip -> xpLift NoData
                        Required -> xpWrap (GotData, gotData) $ xpElem (niceField fn) (xpickle :: PU a)
                        Optional -> xpWrap (maybe2fd, fd2maybe) $ xpOption (xpElem (niceField fn) (xpickle :: PU a))
            in Const $ \pu ->
                  xpWrap (\(a,b) -> set l a b, \b -> (get l b, b)) $ xpPair pa pu

          foldFunc (Const f) acc = f acc

instance RecordSpec v t => S.JSONSchema (t (FieldData v)) where
  schema _ = foldrf foldFunc S.empty fields
    where fields :: t (Const (Maybe S.Schema))
          fields = zip1_3 mkField recordFields fieldNames (fieldSpecs (undefined :: Proxy v))
          mkField (Field :: Field a) fn spec =
              Const $
                case visibility spec of
                  Skip -> Nothing
                  Required ->
                    Just $ S.field (Data.Text.pack (niceField fn)) False (S.schema (undefined :: Proxy a))
                  Optional ->
                    Just $ S.field (Data.Text.pack (niceField fn)) True (S.schema (undefined :: Proxy a))

          foldFunc (Const (Just f)) ac = f `S.merge` ac
          foldFunc (Const Nothing) ac = ac

instance RecordSpec v t => ToJSON (t (FieldData v)) where
  toJSON t = Object $ foldrf foldFunc Map.empty fields
    where fields :: t (Const (Maybe (Data.Text.Text, Value)))
          fields = zip1_4 mkField recordFields fieldNames t (fieldSpecs (undefined :: Proxy v))

          mkField :: forall a. Field a -> FieldName a -> FieldData v a -> FieldSpec a
                           -> (Const (Maybe (Data.Text.Text, Value))) a
          mkField Field fn val spec =
            let nm = Data.Text.pack $ niceField fn in
            Const $ case visibility spec of
                      Skip -> Nothing
                      Required -> Just (nm, toJSON (gotData val))
                      Optional -> fmap ((,) nm . toJSON) $ fd2maybe val

          foldFunc (Const (Just (key, value))) kmap = Map.insert key value kmap
          foldFunc (Const Nothing) kmap = kmap

instance RecordSpec v t => FromJSON (t (FieldData v)) where
  parseJSON (Object items) = traverse1 unParserM fieldParsers
    where fieldParsers :: t (ParserM (FieldData v))
          fieldParsers = zip1_3 mkField recordFields fieldNames (fieldSpecs (undefined :: Proxy v))

          mkField :: forall a.Field a -> FieldName a -> FieldSpec a -> ParserM (FieldData v) a
          mkField (Field :: Field a) fn spec =
            ParserM $
              case visibility spec of
                Skip -> return NoData
                Required -> GotData <$> (items .: (Data.Text.pack $ niceField fn))
                Optional -> maybe2fd <$> (items .:? (Data.Text.pack $ niceField fn))

  parseJSON _ = mzero
