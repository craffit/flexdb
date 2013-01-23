{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators, KindSignatures
           , OverlappingInstances, ScopedTypeVariables, FlexibleInstances, FlexibleContexts
           , UndecidableInstances, TypeSynonymInstances, TupleSections, TypeFamilies
           , TemplateHaskell, EmptyDataDecls, Rank2Types #-}
module DB.Flex.Query.Core where

import Control.Arrow
import Control.Monad.State hiding (modify, get)
import qualified Control.Monad.State as ST
import DB.Flex.Query.Base (BaseQuery(..), BaseExpr)

-- | Type of queries
data Insert
data Value
data Sort
data Update

-- | Lattice for query types
type family MeetType (a :: *) (b :: *) :: *
type instance MeetType Insert Insert = Insert
type instance MeetType Value  Value  = Value
type instance MeetType Sort   Sort   = Sort
type instance MeetType Update Update = Update
type instance MeetType Value  Insert = Insert
type instance MeetType Value  Sort   = Sort
type instance MeetType Value  Update = Update
type instance MeetType Insert Value  = Insert
type instance MeetType Sort   Value  = Sort
type instance MeetType Update Value  = Update
type instance MeetType Insert Update = Update
type instance MeetType Update Insert = Update

-- | Aggregation type of variables/queries
data Constant
data Single
data Aggr

-- | Query level and parent structure
data Sub a l
data Z

-- data L       = Sub SubAggr L | Z

-- | Lattice for aggregation types
type family MeetAggr (a :: *) (b :: *) :: *
type instance MeetAggr Constant Constant = Constant
type instance MeetAggr Constant Single   = Single
type instance MeetAggr Single   Constant = Single
type instance MeetAggr Single   Single   = Single
type instance MeetAggr Constant Aggr     = Aggr
type instance MeetAggr Aggr     Constant = Aggr
type instance MeetAggr Aggr     Aggr     = Aggr

-- | Expression with phantom types: Querytype, aggregation type, level of toccurrence, type
newtype Exp a i l t = Exp { bExp :: State Int (BaseExpr String) }

{-
setExp :: State Int (BaseExpr String) -> Exp a i l t -> Exp a i l t
setExp s e = e { bExp = s }
-}

-- Haskell-level function!
expEquals :: String -> Exp x i l a -> Bool
expEquals s = (== s) . fst . flip runState [] . fst . flip runState 0 . bExp

expString :: Exp x i l a -> String
expString = fst . flip runState [] . fst . flip runState 0 . bExp

pureExp :: String -> Exp x i l a
pureExp = Exp . return . return

runExp :: Exp a i l t -> BaseExpr String
runExp = fst . flip runState 0 . bExp

-- | Short-hands for most occurring types
type Expr         = Exp Value
type InsertExpr i = Exp Insert i Z
type SortExpr     = Exp Sort
type UpdateExpr   = Exp Update Single Z

type ConstantExpr = Exp Value Constant
type SingleExpr   = Exp Value Single
type AggrExpr     = Exp Value Aggr

type Alias      = Int
data QState t l = QState { alias :: Alias
                         , build :: BaseQuery
                         }

type Query t l a    = State (QState t l) a

castExp :: Exp v i l a -> Exp v' i' l' a'
castExp = Exp . bExp

{-
castQState :: QState t l -> QState t' l'
castQState (QState a b) = QState a b

castQuery :: Query t l a -> Query t' l' a
castQuery q = StateT $ fmap (second castQState) . runStateT q . castQState
-}

newAlias :: Query t l Int
newAlias = ST.modify (\(QState a b) -> QState (a + 1) b) >> gets alias

updateBaseQuery :: (BaseQuery -> BaseQuery) -> Query t l ()
updateBaseQuery f = ST.modify $ \(QState a b) -> QState a (f b)

queryExp :: State Int (BaseExpr String) -> Query i' l' (BaseExpr String)
queryExp e =
  do st <- ST.get
     let (ex, a) = runState e (alias st)
     put (st { alias = a })
     return ex


runQueryAlias :: Int -> Query i l a -> (a, QState i l)
runQueryAlias a = flip runState (QState a $ BaseQuery [] (return "true") (return "true") Nothing Nothing False [] [] Nothing)

runQuery :: Query i l a -> (a, BaseQuery)
runQuery = second build . runQueryAlias 0

runSubQuery :: Query i l a -> Query i' l' (a, BaseQuery)
runSubQuery q =
  do QState a bq <- ST.get
     let (r, QState a' bq') = runQueryAlias a q
     put (QState a' bq)
     return (r, bq')

expQuery :: Query i' l' a -> State Int (a, BaseQuery)
expQuery q =
  do a <- ST.get
     let (r, QState a' bq) = runQueryAlias a q
     put a'
     return (r, bq)