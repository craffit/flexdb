{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.TH.Util where

import Language.Haskell.TH

getCons :: Con -> (Name, [Type])
getCons (NormalC cnm tps) = (cnm, map snd tps)
getCons (RecC cnm tps) = (cnm, map (\(_,_,t) -> t) tps)
getCons c = error $ "Invalid constructor in getCons: " ++ show c

getTV :: TyVarBndr -> Name
getTV (PlainTV n) = n
getTV (KindedTV n _) = n

withTyConReify :: (Dec -> Q [Dec]) -> Name -> Q [Dec]
withTyConReify f nm =
  do TyConI d <- reify nm
     f d
