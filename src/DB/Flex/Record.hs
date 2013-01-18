{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
  , TypeSynonymInstances, FlexibleInstances, UndecidableInstances
  , OverlappingInstances, TypeFamilies, Rank2Types, KindSignatures
  , TypeOperators, LiberalTypeSynonyms, FlexibleContexts, ScopedTypeVariables
  , GADTs, TemplateHaskell, PolyKinds, StandaloneDeriving  #-}
-----------------------------------------------------------
-- |
-- Module      :  HDBRec
-- Copyright   :  HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
--
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This is a replacement for some of TREX.
--
--
-----------------------------------------------------------
module DB.Flex.Record where


import Control.Monad.Identity

import Data.Char
import Data.Convertible
-- import Data.Label.Util
import Data.Maybe
import Database.HDBC

import Data.Record.Abstract
import Data.Foldable1
import Data.Functor1
import Data.Traversable1
import Data.Zippable1

import DB.Flex.Config
import DB.Flex.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Util
{-
newtype Lens1 t a = Lens1 { unLens1 :: forall f. t f :-> f a }
newtype Val1 (t :: * -> *) a = Val1 { unVal1 :: t a }
-}

data Tup1 f g a = Tup1 (f a) (g a)


-- * Database fields
data FieldName a = FieldName { unFieldName :: String }

data Field a where
  Field :: (Convertible a SqlValue, Convertible SqlValue a) => Field a

{-
data FieldOpt a where
  Nullable :: FieldOpt (Maybe a)
  Default  :: a -> FieldOpt a
  Primary  :: FieldOpt a
  Unique   :: FieldOpt a
  Foreign  :: DBTable t => t |> a -> FieldOpt a

data FieldOpts a = FieldOpts { fieldOpts :: [FieldOpt a] }
-}

class (Zippable1 r, Traversable1 r) => DBRecord r where
  recordFields :: r Field

class DBRecord t => DBTable t where
  tableName    :: t v -> String
  fieldNames   :: t FieldName
--  fieldOptions :: t FieldOpts
--  fieldOptions = fmap1 (\_ -> FieldOpts []) recordFields

instance (Convertible a SqlValue, Convertible SqlValue a) => DBRecord (AbstractVal a) where
  recordFields = AbstractVal Field

buildRecord :: DBRecord r => [SqlValue] -> r Identity
buildRecord vs = distribute (\Field v -> Identity $ fromSql v) vs recordFields

recordValues :: DBRecord r => r Identity -> [SqlValue]
recordValues = collect (\(Tup1 Field v) -> toSql $ runIdentity v) . zip1 Tup1 recordFields

names :: forall t v. DBTable t => t v -> [String]
names _ = foldrf (\v ac -> unFieldName v : ac) [] (fieldNames :: t FieldName)

mkRecordFields :: Name -> Q [Dec]
mkRecordFields = withTyConReify mkRecordFields'

mkRecordFields' :: Dec -> Q [Dec]
mkRecordFields' (DataD _ tnm pars [con] _) =
  let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
      (cnm, tps) = getCons con
      cls = Clause [] (NormalB $ foldl AppE (ConE cnm) $ map mkType tps) []
      mkType (AppT tf a) | tf == VarT f = ConE (mkName "Field")
                         | a == VarT f  = VarE (mkName "recordFields")
      mkType _ = error "Not a valid field"
  in return [InstanceD
                 []
                 (ConT (mkName "DBRecord") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
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

{-
-- | Generate table instance from a datatype

mkTableFields :: Dec -> Q [Dec]
mkTableFields dc =
  do 
     let anm = (\(DataD _ v _ _ _) -> v) $ head rec
         inst = InstanceD [] (ConT (mkName "DBTable") `AppT` ConT anm)
                   [FunD (mkName "tableName") [Clause [WildP] (NormalB $ LitE $ StringL tbl) []]
                   ,FunD (mkName "fieldNames")
                       [Clause [] (NormalB $ foldl AppE (ConE anm) $ map (AppE (ConE $ mkName "FieldName") . LitE . StringL) $ map fst cols) []]
                   ]
     return $ [ inst ]
-}

-- | Generate a table datatype from a remote Postgres database

retrieveTable :: String -> String -> (String -> String) -> (SqlTypeId -> String) -> Config -> Q [Dec]
retrieveTable tbl hnm mkI mkT conf =
  do cols <- runIO $ withConnection conf $ flip describeTable tbl
     let mkCol (col, cDesc) = addNullable (fromMaybe True $ colNullable cDesc)
                                      (mkName $ mkI col, NotStrict, ConT $ mkName $ mkT $ colType cDesc)
         addNullable True  (n, s, t) = (n, s, AppT (ConT $ mkName "Maybe") t)
         addNullable False v         = v
     let dat  = DataD [] (mkName hnm) [] [RecC (mkName hnm) (map mkCol cols)] [mkName "Show", mkName "Eq"]
     rec <- dbRecord' dat
     let anm = (\(DataD _ v _ _ _) -> v) $ head rec
         inst = InstanceD [] (ConT (mkName "DBTable") `AppT` ConT anm)
                   [FunD (mkName "tableName") [Clause [WildP] (NormalB $ LitE $ StringL tbl) []]
                   ,FunD (mkName "fieldNames")
                       [Clause [] (NormalB $ foldl AppE (ConE anm) $ map (AppE (ConE $ mkName "FieldName") . LitE . StringL) $ map fst cols) []]
                   ]
     return $ [ dat, inst ] ++ rec

-- | Mapping SQL types to Haskell types
baseTypeInfo :: SqlTypeId -> String
baseTypeInfo SqlCharT              = "String"     
baseTypeInfo SqlVarCharT           = "String"     
baseTypeInfo SqlLongVarCharT       = "String"     
baseTypeInfo SqlWCharT             = "String"     
baseTypeInfo SqlWVarCharT          = "String"     
baseTypeInfo SqlWLongVarCharT      = "String"     
baseTypeInfo SqlDecimalT           = "Integer"    
baseTypeInfo SqlNumericT           = "Integer" 
baseTypeInfo SqlSmallIntT          = "Int"     
baseTypeInfo SqlIntegerT           = "Int"     
baseTypeInfo SqlRealT              = "Double"
baseTypeInfo SqlFloatT             = "Double"  
baseTypeInfo SqlDoubleT            = "Double"  
baseTypeInfo SqlBitT               = "Bool"    
baseTypeInfo SqlTinyIntT           = "Int"     
baseTypeInfo SqlBigIntT            = "Integer"    
baseTypeInfo SqlBinaryT            = "ByteString" 
baseTypeInfo SqlVarBinaryT         = "ByteString" 
baseTypeInfo SqlLongVarBinaryT     = "ByteString" 
baseTypeInfo SqlDateT              = "UTCTime"    
baseTypeInfo SqlTimeT              = "UTCTime"    
baseTypeInfo SqlTimeWithZoneT      = "UTCTime"    
baseTypeInfo SqlTimestampT         = "UTCTime"    
baseTypeInfo SqlTimestampWithZoneT = "UTCTime"    
baseTypeInfo SqlUTCDateTimeT       = "UTCTime"    
baseTypeInfo SqlUTCTimeT           = "UTCTime"    
baseTypeInfo (SqlIntervalT _)      = "SqlValue" 
baseTypeInfo SqlGUIDT              = "UUID"     
baseTypeInfo (SqlUnknownT "2950")  = "UUID"
baseTypeInfo (SqlUnknownT _)       = "SqlValue" 

withCase :: Eq a => a -> r -> (a -> r) -> a -> r
withCase a r f a' | a' == a = r
                  | otherwise = f a'

baseIdent :: String -> String
baseIdent = ("_" ++) . firstDown . mkid

firstDown :: String -> String
firstDown [] = []
firstDown (x:xs) = toLower x: xs

mkid :: String -> String
mkid "" = ""
mkid ('_' : x : xs) = toUpper x : mkid xs
mkid (x : xs) | isAlphaNum x = x : mkid xs
              | otherwise    = mkid xs
