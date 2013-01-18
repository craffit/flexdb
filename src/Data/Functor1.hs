{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Data.Functor1 where

import Control.Arrow

import Language.Haskell.TH
import Language.Haskell.TH.Util

class Functor1 f where
  fmap1 :: (forall a. g a -> h a) -> f g -> f h

mkFunctor1 :: Name -> Q [Dec]
mkFunctor1 = withTyConReify mkFunctor1'

mkFunctor1' :: Dec -> Q [Dec]
mkFunctor1' (DataD _ tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
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
mkFunctor1' _ = error "Can only derive Functor1 for data type"