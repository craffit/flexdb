{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Data.Zippable1 where

import Control.Arrow

import Language.Haskell.TH
import Language.Haskell.TH.Util

class Zippable1 t where
  zip1 :: (forall a. f a -> g a -> h a) -> t f -> t g -> t h

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