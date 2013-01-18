{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Data.Foldable1 where

import Control.Arrow

import Data.Functor1
import Language.Haskell.TH
import Language.Haskell.TH.Util

class Functor1 f => Foldable1 f where
  foldrf :: (forall a. g a -> r -> r) -> r -> f g -> r

collect :: Foldable1 f => (forall a. g a -> v) -> f g -> [v]
collect f = foldrf (\v ac -> f v : ac) []

mkFoldable1 :: Name -> Q [Dec]
mkFoldable1 = withTyConReify mkFoldable1'

mkFoldable1' :: Dec -> Q [Dec]
mkFoldable1' (DataD _ tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
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
mkFoldable1' _ = error "Can only derive Foldable1 for data type"