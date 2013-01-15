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
module DB.Flex.Record
   ( Functor1(..), Foldable1(..), Traversable1(..), Zippable1(..)
   , liftIdent, collect, distribute

   , AbstractType(..), DBRecord(..), DBTable(..)
   , FieldName(..), Field(..), names

   , AbstractVal(..), realVal, Pair1(..), Lens1(..), Val1(..)

   , mkAbstractType, mkFunctor1, mkFoldable1, mkTraversable1, mkZippable1, mkRecordFields, dbRecord, mkTable

   , baseTypeInfo, withCase, baseIdent
   , recordValues, buildRecord
   
   , module Control.Applicative
   , module Control.Monad.Identity
   , SqlValue
   )
   where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as ST

import Data.Char
import Data.Convertible
import Data.Label
import qualified Data.Label.Abstract as A
import Data.Maybe
import Database.HDBC
import Data.UUID

import DB.Flex.Config
import DB.Flex.Monad

import Language.Haskell.TH

deriving instance Show a => Show (Identity a)
deriving instance Eq a => Eq (Identity a)

class Functor1 f where
  fmap1 :: (forall a. g a -> h a) -> f g -> f h

class Functor1 f => Foldable1 f where
  foldrf :: (forall a. g a -> r -> r) -> r -> f g -> r

class (Functor1 t, Foldable1 t) => Traversable1 t where
  traverse1 :: Applicative m => (forall a. f a -> m (g a)) -> t f -> m (t g)

liftIdent :: (Traversable1 t, Monad m, Applicative m) => t m -> m (t Identity)
liftIdent = traverse1 (liftM Identity)

collect :: Foldable1 f => (forall a. g a -> v) -> f g -> [v]
collect f = foldrf (\v ac -> f v : ac) []

distribute :: forall r f g v. Traversable1 r => (forall a. f a -> v -> g a) -> [v] -> r f -> r g
distribute f vs = flip evalState vs . traverse1 step
  where step :: forall a. f a -> State [v] (g a)
        step a = do v <- ST.get
                    case v of
                      []      -> error "Not enough values to distribute"
                      (x: xs) -> put xs >> return (f a x)

class Zippable1 t where
  zip1 :: (forall a. f a -> g a -> h a) -> t f -> t g -> t h

newtype Lens1 t a = Lens1 { unLens1 :: forall f. t f :-> f a }
newtype Val1 (t :: * -> *) a = Val1 { unVal1 :: t a }
data Tup1 f g a = Tup1 (f a) (g a)

class AbstractType v a | v -> a, a -> v where
  toAbstract    :: v -> a Identity
  fromAbstract  :: a Identity -> v
{-
  realLenses    :: a (Val1 (A.Lens (->) v))
  absLenses     :: a (Lens1 a)
-}

-- * Database fields
data FieldName a = FieldName { unFieldName :: String }

data Field a where
  Field :: (Convertible a SqlValue, Convertible SqlValue a) => Field a

class (Zippable1 r, Traversable1 r) => DBRecord r where
  recordFields :: r Field

class DBRecord t => DBTable t where
  tableName  :: t v -> String
  fieldNames :: t FieldName

buildRecord :: DBRecord r => [SqlValue] -> r Identity
buildRecord vs = distribute (\Field v -> Identity $ fromSql v) vs recordFields

recordValues :: DBRecord r => r Identity -> [SqlValue]
recordValues = collect (\(Tup1 Field v) -> toSql $ runIdentity v) . zip1 Tup1 recordFields

names :: forall t v. DBTable t => t v -> [String]
names _ = foldrf (\v ac -> unFieldName v : ac) [] (fieldNames :: t FieldName)

data AbstractVal a f = AbstractVal { _realVal :: f a } deriving (Show, Eq)

$( mkLabels [''AbstractVal] )

instance Functor1 (AbstractVal a) where
  fmap1 f = AbstractVal . f . get realVal

instance Foldable1 (AbstractVal a) where
  foldrf f b (AbstractVal v) = f v b

instance Zippable1 (AbstractVal a) where
  zip1 f (AbstractVal a) (AbstractVal b) = AbstractVal (f a b)

instance Traversable1 (AbstractVal a) where
  traverse1 f (AbstractVal v) = AbstractVal <$> f v

instance (Convertible a SqlValue, Convertible SqlValue a) => DBRecord (AbstractVal a) where
  recordFields = AbstractVal Field

{-
instance AbstractType (r Identity) r where
  toAbstract = id
  fromAbstract = id
-}

instance AbstractType Int (AbstractVal Int) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal
{-
  realLenses   = AbstractVal $ Val1 $ lens id const
  absLenses    = AbstractVal $ Lens1 realVal
-}

instance AbstractType String (AbstractVal String) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal
{-
  realLenses   = AbstractVal $ Val1 $ lens id const
  absLenses    = AbstractVal $ Lens1 realVal
-}

instance AbstractType UUID (AbstractVal UUID) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal
{-
  realLenses   = AbstractVal $ Val1 $ lens id const
  absLenses    = AbstractVal $ Lens1 realVal
-}

instance AbstractType Integer (AbstractVal Integer) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal
{-
  realLenses   = AbstractVal $ Val1 $ lens id const
  absLenses    = AbstractVal $ Lens1 realVal
-}

instance AbstractType Float (AbstractVal Float) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal
{-
  realLenses   = AbstractVal $ Val1 $ lens id const
  absLenses    = AbstractVal $ Lens1 realVal
-}
instance AbstractType Double (AbstractVal Double) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal
{-
  realLenses   = AbstractVal $ Val1 $ lens id const
  absLenses    = AbstractVal $ Lens1 realVal
-}

data Pair1 a b (f :: * -> *) = Pair1 { _fst1 :: a f, _snd1 :: b f }

$( mkLabels [''Pair1] )

instance (Functor1 a, Functor1 b) => Functor1 (Pair1 a b) where
  fmap1 f (Pair1 a b) = Pair1 (fmap1 f a) (fmap1 f b)

instance (Foldable1 a, Foldable1 b) => Foldable1 (Pair1 a b) where
  foldrf f v (Pair1 a b) = foldrf f (foldrf f v a) b

instance (Zippable1 a, Zippable1 b) => Zippable1 (Pair1 a b) where
  zip1 f (Pair1 a b) (Pair1 c d) = Pair1 (zip1 f a c) (zip1 f b d)

instance (Traversable1 a, Traversable1 b) => Traversable1 (Pair1 a b) where
  traverse1 f (Pair1 a b) = Pair1 <$> traverse1 f a <*> traverse1 f b

instance (DBRecord a, DBRecord b) => DBRecord (Pair1 a b) where
  recordFields = Pair1 recordFields recordFields

instance (AbstractType a a', AbstractType b b') => AbstractType (a , b) (Pair1 a' b') where
  toAbstract   (a,b)       = Pair1 (toAbstract a) (toAbstract b)
  fromAbstract (Pair1 a b) = (fromAbstract a, fromAbstract b)

getCons :: Con -> (Name, [Type])
getCons (NormalC cnm tps) = (cnm, map snd tps)
getCons (RecC cnm tps) = (cnm, map (\(_,_,t) -> t) tps)

getTV :: TyVarBndr -> Name
getTV (PlainTV n) = n
getTV (KindedTV n _) = n

withTyConReify :: (Dec -> Q [Dec]) -> Name -> Q [Dec]
withTyConReify f nm =
  do TyConI d <- reify nm
     f d

mkAbstractType :: Name -> Q [Dec]
mkAbstractType = withTyConReify mkAbstractType'

mkAbstractType' :: Dec -> Q [Dec]
mkAbstractType' (DataD ctx tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..30]
     let aName nm = mkName $ nameBase nm ++ "'"
         aField nm = mkName $ nameBase nm ++ "'"
         parNames = map getTV pars
         appType cnm = foldl AppT (ConT cnm) . map VarT
         f = mkName "f"
         mkCons (NormalC cnm flds) = NormalC (aName cnm) $ map (\(st,t) -> (st, (VarT f) `AppT` t)) flds
         mkCons (RecC cnm flds)    = RecC (aName cnm) $ map (\(fnm,st,t) -> (aField fnm, st, (VarT f) `AppT` t)) flds
         toClause con =
           let (cnm, tps) = getCons con
           in Clause [ConP cnm $ map VarP $ take (length tps) names]
                     (NormalB $ foldl AppE (ConE $ aName cnm) $ map (AppE (ConE (mkName "Identity")) . VarE) $ take (length tps) names) []
         fromClause con =
           let (cnm, tps) = getCons con
           in Clause [ConP (aName cnm) $ map VarP $ take (length tps) names]
                     (NormalB $ foldl AppE (ConE cnm) $ map (AppE (VarE (mkName "runIdentity")) . VarE) $ take (length tps) names) []
     return [ DataD ctx (aName tnm) (pars ++ [KindedTV f $ AppT (AppT ArrowT StarT) StarT]) (map mkCons cons) []
            , InstanceD []
                (ConT (mkName "AbstractType") `AppT` appType tnm parNames `AppT` appType (aName tnm) parNames)
                [ FunD (mkName "toAbstract") (map toClause cons)
                , FunD (mkName "fromAbstract") (map fromClause cons)
                ]
            ]

mkFunctor1 :: Name -> Q [Dec]
mkFunctor1 = withTyConReify mkFunctor1'

mkFunctor1' :: Dec -> Q [Dec]
mkFunctor1' (DataD _ tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..30]
     let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
         parName = mkName "f"
         mkCons c =
           let (cnm, tps) = getCons c
               (insts, exps) = unzip $ zipWith mkType names tps
           in ( concat insts
              , Clause
                  [VarP parName, ConP cnm $ map VarP $ take (length exps) names]
                  (NormalB $ foldl AppE (ConE cnm) exps)
                  []
              )
         mkType vnm (AppT tf a) | tf == VarT f = ([], VarE parName `AppE` VarE vnm)
                                | a == VarT f  = ([tf], VarE (mkName "fmap1") `AppE` VarE parName `AppE` VarE vnm)
         mkType vnm _ = ([], VarE vnm)
         (preds, clauses) = first concat $ unzip $ map mkCons cons
     return [InstanceD
                 (map (ClassP (mkName "Functor1") . (:[])) preds)
                 (ConT (mkName "Functor1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                 [FunD (mkName "fmap1") clauses]
            ]

mkFoldable1 :: Name -> Q [Dec]
mkFoldable1 = withTyConReify mkFoldable1'

mkFoldable1' :: Dec -> Q [Dec]
mkFoldable1' (DataD _ tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..30]
     let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
         funcName = mkName "f"
         baseName = mkName "b"
         mkCons c =
           let (cnm, tps) = getCons c
               (insts, exps) = unzip $ zipWith mkType names tps
           in ( concat insts
              , Clause
                  [VarP funcName, VarP baseName, ConP cnm $ map VarP $ take (length exps) names]
                  (NormalB $ foldr AppE (VarE baseName) exps)
                  []
              )
         mkType vnm (AppT tf a) | tf == VarT f = ([], VarE funcName `AppE` VarE vnm)
                                | a == VarT f  = ([tf], VarE (mkName "foldrf") `AppE` VarE funcName `AppE` VarE vnm)
         mkType vnm _ = ([], VarE vnm)
         (preds, clauses) = first concat $ unzip $ map mkCons cons
     return [InstanceD
                 (map (ClassP (mkName "Foldable1") . (:[])) preds)
                 (ConT (mkName "Foldable1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                 [FunD (mkName "foldrf") clauses]
            ]

mkTraversable1 :: Name -> Q [Dec]
mkTraversable1 = withTyConReify mkTraversable1'

mkTraversable1' :: Dec -> Q [Dec]
mkTraversable1' (DataD _ tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..30]
     let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
         funcName = mkName "f"
         mkCons c =
           let (cnm, tps) = getCons c
               (insts, exps) = unzip $ zipWith mkType names tps
           in ( concat insts
              , Clause
                  [VarP funcName, ConP cnm $ map VarP $ take (length exps) names]
                  (NormalB $ foldl (\l r -> InfixE (Just l) (VarE $ mkName "<*>") (Just r)) (VarE (mkName "pure") `AppE` ConE cnm) exps)
                  []
              )
         mkType vnm (AppT tf a) | tf == VarT f = ([], VarE funcName `AppE` VarE vnm)
                                | a == VarT f  = ([tf], VarE (mkName "traverse1") `AppE` VarE funcName `AppE` VarE vnm)
         mkType vnm _ = ([], VarE vnm)
         (preds, clauses) = first concat $ unzip $ map mkCons cons
     return [InstanceD
                 (map (ClassP (mkName "Traversable1") . (:[])) preds)
                 (ConT (mkName "Traversable1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                 [FunD (mkName "traverse1") clauses]
            ]

mkZippable1 :: Name -> Q [Dec]
mkZippable1 = withTyConReify mkZippable1'

mkZippable1' :: Dec -> Q [Dec]
mkZippable1' (DataD _ tnm pars [con] _) =
  do names  <- mapM newName $ map (('v':) . show) [1..30]
     names2 <- mapM newName $ map (('v':) . show) [1..30]
     let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
         (cnm, tps) = getCons con
         funcName = mkName "f"
         mkType (vn1, vn2) (AppT tf a) | tf == VarT f = ([], VarE funcName `AppE` VarE vn1 `AppE` VarE vn2)
                                       | a == VarT f  = ([tf], VarE (mkName "zip1") `AppE` VarE funcName `AppE` VarE vn1 `AppE` VarE vn2)
         mkType _ _ = error "Not a valid field type"
         (preds, exps) = first concat . unzip $ zipWith mkType (zip names names2) tps
         clause = Clause
                    [ VarP funcName, ConP cnm $ map VarP $ take (length exps) names
                    , ConP cnm $ map VarP $ take (length exps) names2 ]
                    (NormalB $ foldl AppE (ConE cnm) exps) []

     return [InstanceD
                  (map (ClassP (mkName "Zippable1") . (:[])) preds)
                  (ConT (mkName "Zippable1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                  [FunD (mkName "zip1") [clause]]
            ]

mkRecordFields :: Name -> Q [Dec]
mkRecordFields = withTyConReify mkRecordFields'

mkRecordFields' :: Dec -> Q [Dec]
mkRecordFields' (DataD _ tnm pars [con] _) =
  let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
      (cnm, tps) = getCons con
      clause = Clause [] (NormalB $ foldl AppE (ConE cnm) $ map mkType tps) []
      mkType (AppT tf a) | tf == VarT f = ConE (mkName "Field")
                         | a == VarT f  = VarE (mkName "recordFields")
      mkType _ = error "Not a valid field"
  in return [InstanceD
                 []
                 (ConT (mkName "DBRecord") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                 [FunD (mkName "recordFields") [clause]]
            ]

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

mkTable :: String -> String -> (String -> String) -> (SqlTypeId -> String) -> Config -> Q [Dec]
mkTable tbl hnm mkI mkT conf =
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
