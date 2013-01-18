{-# LANGUAGE Rank2Types, TemplateHaskell, ScopedTypeVariables #-}
module Data.Traversable1 where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.Foldable1
import Data.Functor1

import Language.Haskell.TH
import Language.Haskell.TH.Util

class (Functor1 t, Foldable1 t) => Traversable1 t where
  traverse1 :: Applicative m => (forall a. f a -> m (g a)) -> t f -> m (t g)

distribute :: forall r f g v. Traversable1 r => (forall a. f a -> v -> g a) -> [v] -> r f -> r g
distribute f vs = flip evalState vs . traverse1 step
  where step :: forall a. f a -> State [v] (g a)
        step a = do v <- get
                    case v of
                      []      -> error "Not enough values to distribute"
                      (x: xs) -> put xs >> return (f a x)

liftIdent :: (Traversable1 t, Monad m, Applicative m) => t m -> m (t Identity)
liftIdent = traverse1 (liftM Identity)

mkTraversable1 :: Name -> Q [Dec]
mkTraversable1 = withTyConReify mkTraversable1'

mkTraversable1' :: Dec -> Q [Dec]
mkTraversable1' (DataD _ tnm pars cons _) =
  do names <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
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
mkTraversable1' _ = error "Can only derive Traversable1 for data type"