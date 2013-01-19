{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell
           , StandaloneDeriving, KindSignatures, FlexibleInstances, CPP #-}
module Data.Record.Abstract where

import Control.Monad.Identity

import Data.Functor
import Data.Label

import Data.UUID
import Data.Functor1
import Data.Foldable1
import Data.Traversable1
import Data.Zippable1

import Language.Haskell.TH
import Language.Haskell.TH.Util

class AbstractType v a | v -> a, a -> v where
  toAbstract    :: v -> a Identity
  fromAbstract  :: a Identity -> v

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


instance AbstractType Int (AbstractVal Int) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal

instance AbstractType String (AbstractVal String) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal

instance AbstractType UUID (AbstractVal UUID) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal

instance AbstractType Integer (AbstractVal Integer) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal

instance AbstractType Float (AbstractVal Float) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal

instance AbstractType Double (AbstractVal Double) where
  toAbstract   = AbstractVal . Identity
  fromAbstract = runIdentity . get realVal

{-
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
-}

deriving instance Show a => Show (Identity a)
deriving instance Eq a => Eq (Identity a)

mkAbstractType :: Name -> Q [Dec]
mkAbstractType = withTyConReify mkAbstractType'

mkAbstractType' :: Dec -> Q [Dec]
mkAbstractType' (DataD ctx tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
     let aName nm = mkName $ nameBase nm ++ "'"
         aField nm = mkName $ nameBase nm ++ "'"
         parNames = map getTV pars
         appType cnm = foldl AppT (ConT cnm) . map VarT
         f = mkName "f"
         mkCons (NormalC cnm flds) = NormalC (aName cnm) $ map (\(st,t) -> (st, (VarT f) `AppT` t)) flds
         mkCons (RecC cnm flds)    = RecC (aName cnm) $ map (\(fnm,st,t) -> (aField fnm, st, (VarT f) `AppT` t)) flds
         mkCons x = error $ "Cannot handle constructor " ++ show x ++ " in mkAbstractType'"
         toClause con =
           let (cnm, tps) = getCons con
           in Clause [ConP cnm $ map VarP $ take (length tps) names]
                     (NormalB $ foldl AppE (ConE $ aName cnm) $ map (AppE (ConE (mkName "Identity")) . VarE) $ take (length tps) names) []
         fromClause con =
           let (cnm, tps) = getCons con
           in Clause [ConP (aName cnm) $ map VarP $ take (length tps) names]
                     (NormalB $ foldl AppE (ConE cnm) $ map (AppE (VarE (mkName "runIdentity")) . VarE) $ take (length tps) names) []
#if __GLASGOW_HASKELL__==706
     return [ DataD ctx (aName tnm) (pars ++ [KindedTV f $ AppT (AppT ArrowT StarT) StarT]) (map mkCons cons) []
#else
     return [ DataD ctx (aName tnm) (pars ++ [KindedTV f $ ArrowK StarK StarK]) (map mkCons cons) []
#endif
            , InstanceD []
                (ConT (mkName "AbstractType") `AppT` appType tnm parNames `AppT` appType (aName tnm) parNames)
                [ FunD (mkName "toAbstract") (map toClause cons)
                , FunD (mkName "fromAbstract") (map fromClause cons)
                ]
            ]
mkAbstractType' _ = error "Can only create an abstract type for Data declarations"