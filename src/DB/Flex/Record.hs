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

import Data.Convertible
import Database.HDBC

import Data.Record.Abstract
import Data.Foldable1
import Data.Functor1
import Data.Traversable1
import Data.Zippable1

import Language.Haskell.TH
import Language.Haskell.TH.Util

data Tup1 f g a = Tup1 (f a) (g a)
data ConstVal a b = ConstVal { unConstVal :: a }

-- * Database fields
data FieldName a = FieldName { unFieldName :: String }

data Field a where
  Field :: (Convertible a SqlValue, Convertible SqlValue a) => Field a

class (Zippable1 r, Traversable1 r) => DBRecord r where
  recordFields :: r Field

class DBRecord t => Table t where
  tableName    :: t v -> String
  fieldNames   :: t FieldName

instance (Convertible a SqlValue, Convertible SqlValue a) => DBRecord (AbstractVal a) where
  recordFields = AbstractVal Field

buildRecord :: DBRecord r => [SqlValue] -> r Identity
buildRecord vs = distribute (\Field v -> Identity $ fromSql v) vs recordFields

recordValues :: DBRecord r => r Identity -> [SqlValue]
recordValues = collect (\(Tup1 Field v) -> toSql $ runIdentity v) . zip1 Tup1 recordFields

names :: forall t v. Table t => t v -> [String]
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
