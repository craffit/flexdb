{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Data.Zippable1 where

import Control.Arrow
import Data.Functor1

import Language.Haskell.TH
import Language.Haskell.TH.Util

class Zippable1 t where
  zip1 :: (forall a. f a -> g a -> h a) -> t f -> t g -> t h

data Pair1 f g a = Pair1 { unPair1 :: (f a, g a) }

mkPair1 :: f a -> g a -> Pair1 f g a
mkPair1 f g = Pair1 (f, g)

zip1_3 :: (Functor1 t, Zippable1 t) =>
  (forall a. f a -> g a -> h a -> i a) -> t f -> t g -> t h -> t i
zip1_3 z fa ga ha =
      fmap1 (\(Pair1 (f,Pair1 (g,h))) -> z f g h)
    $ zip1 mkPair1 fa (zip1 mkPair1 ga ha)

zip1_4 :: (Functor1 t, Zippable1 t) =>
  (forall a. f a -> g a -> h a -> i a -> j a) -> t f -> t g -> t h -> t i -> t j
zip1_4 z fa ga ha ia =
      fmap1 (\(Pair1 (Pair1 (f, g), Pair1 (h,i))) -> z f g h i)
    $ zip1 mkPair1 (zip1 mkPair1 fa ga) (zip1 mkPair1 ha ia)

mkZippable1 :: Name -> Q [Dec]
mkZippable1 = withTyConReify mkZippable1'

mkZippable1' :: Dec -> Q [Dec]
mkZippable1' (DataD _ tnm pars [con] _) =
  do names  <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
     names2 <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
     let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
         (cnm, tps) = getCons con
         funcName = mkName "f"
         mkType (vn1, vn2) (AppT tf a) | tf == VarT f = ([], VarE funcName `AppE` VarE vn1 `AppE` VarE vn2)
                                       | a == VarT f  = ([tf], VarE (mkName "zip1") `AppE` VarE funcName `AppE` VarE vn1 `AppE` VarE vn2)
         mkType _ _ = error "Not a valid field type"
         (preds, exps) = first concat . unzip $ zipWith mkType (zip names names2) tps
         cls = Clause
                    [ VarP funcName, ConP cnm $ map VarP $ take (length exps) names
                    , ConP cnm $ map VarP $ take (length exps) names2 ]
                    (NormalB $ foldl AppE (ConE cnm) exps) []

     return [InstanceD
                  (map (ClassP (mkName "Zippable1") . (:[])) preds)
                  (ConT (mkName "Zippable1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                  [FunD (mkName "zip1") [cls]]
            ]
mkZippable1' _ = error "Can only derive Zippable1 for data type"
