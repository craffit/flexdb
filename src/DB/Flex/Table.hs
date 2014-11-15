{-# LANGUAGE TemplateHaskell, GADTs, TypeOperators, Rank2Types, ScopedTypeVariables, KindSignatures
           , OverlappingInstances, FlexibleInstances, UndecidableInstances
           #-}
module DB.Flex.Table where

import Control.Applicative
import Control.Monad

import Data.Maybe

import Data.Functor1
import Data.Foldable1
import Data.Zippable1
import Data.Label.Util

import DB.Flex.Record
import DB.Flex.Query.Base (binOp)
import DB.Flex.Query.Core
import DB.Flex.Query.Typed

data Action = None | Restrict | SetNull | SetDefault | Cascade deriving Eq

instance Show Action where
  show None       = "no action"
  show Restrict   = "restrict"
  show SetNull    = "set null"
  show SetDefault = "set default"
  show Cascade    = "cascade"

data FieldOpt a  where
  Nullable :: FieldOpt (Maybe a)
  NotNull  :: FieldOpt a
  Def      :: (MeetAggr i Single ~ Single) => Expr i l a -> FieldOpt a
  Primary  :: FieldOpt a
  Unique   :: FieldOpt a
  Foreign  :: Table t => t :> a -> Action -> FieldOpt a
  ForeignMaybe :: Table t => t :> a -> Action -> FieldOpt (Maybe a)
  Type     :: String -> FieldOpt a
  Check    :: (SingleExpr l a -> SingleExpr l Bool) -> FieldOpt a

isNullable :: FieldOpt a -> Bool
isNullable Nullable = True
isNullable _ = False

instance Eq (FieldOpt a) where
  Nullable  == Nullable = True
  NotNull   == NotNull  = True
  (Def a)   == (Def b)  = expString a == expString b
  Primary   == Primary  = True
  Unique    == Unique   = True
  (Foreign (l :: t :> x) a) == (Foreign (l' :: t' :> x') a')  =
    let nms  = fieldNames :: t FieldName
        nms' = fieldNames :: t' FieldName
    in a == a' && tableName nms == tableName nms' && unFieldName (nms |.| l) == unFieldName (nms' |.| l')
  (Type a)  == (Type b) = a == b
  (Check f) == (Check f') = expString (f (pureExp "field")) == expString (f' (pureExp "field"))
  _ == _ = False

newtype FieldOpts a = FieldOpts [FieldOpt a]

data TableOpt t where
  TableUnique  :: [Label t] -> TableOpt t
  TablePrimary :: [Label t] -> TableOpt t
  TableCheck   :: (t (SingleExpr l) -> SingleExpr l Bool) -> TableOpt t

-- | Class for table deifnitions. Can be used to create table on a remote database and to derive efficient queries for table records.
class Table t => TableDef t where
  tableOpts :: [TableOpt t]
  tableOpts = []
  fieldOpts :: t FieldOpts
  fieldOpts = fmap1 (const $ FieldOpts []) fieldNames

-- | It is always possible to derive an empty table defintion without any specifics. This type class can be overrridden for specific cases.
instance Table t => TableDef t

-- | Query a table record using a table definition. This function will either use a unique or primary key, a unique constraint or the entire row depending on how much data is present.
filterRecord :: forall t i l. TableDef t => t (InsertExpr Single l) -> t (Expr Single l) -> Query i l ()
filterRecord dat q =
  let mkQ :: Exp Insert Single l a -> Expr Single l a -> Query i l ()
      mkQ (Exp e1) (Exp e2) = restrict $ Exp $ binOp "=" <$> e1 <*> e2

      fieldQ :: Tup1 (InsertExpr Single l) (Tup1 (Expr Single l) FieldOpts) a -> Maybe (Query i l ())
      fieldQ (Tup1 d (Tup1 e (FieldOpts ops))) =
        if any (flip elem ops) [Unique,Primary] && not (isDefault d)
          then Just $ mkQ d e
          else Nothing

      maybeLabels (TableUnique lbs)  = Just lbs
      maybeLabels (TablePrimary lbs) = Just lbs
      maybeLabels _                  = Nothing

      labelQ lbs =
        if any (\(Label l) -> isDefault (dat |.| l)) lbs
          then Nothing
          else Just $ sequence_ $ map (\(Label l) -> mkQ (dat |.| l) (q |.| l)) lbs

      allQ = if and (collect (not . isDefault) dat)
              then Just $ sequence_ $ collect (\(Tup1 d e) -> mkQ d e) $ zip1 Tup1 dat q
              else Nothing

  in head $ catMaybes $
              collect fieldQ (zip1 Tup1 dat (zip1 Tup1 q fieldOpts))
           ++ map (maybeLabels >=> labelQ) (tableOpts :: [TableOpt t])
           ++ [allQ]
           ++ [Just $ restrict $ con' False]
