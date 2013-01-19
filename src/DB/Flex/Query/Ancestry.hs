{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts
           , TypeOperators, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances
           , Rank2Types, FunctionalDependencies, UndecidableInstances
           , ConstraintKinds, GADTs #-}
module DB.Flex.Query.Ancestry where

import Data.Convertible
import Data.Label.Util
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Typed
import GHC.Prim


class (DBTable t, Eq (RecordKey t), Convertible (RecordKey t) SqlValue) => RecordSelector t where
  type RecordKey t :: *
  keyField :: t :> RecordKey t


class RecordSelector t => PrimarySelector t


class (Eq (JoinKey t), RecordSelector t, RecordSelector (ParentTable t)) => ChildSelector t where
  type ParentTable t :: (* -> *) -> *
  type JoinKey t :: *
  parentJoin :: (ParentTable t :><: t) (JoinKey t)


children :: ChildSelector t => Query i l (ParentTable t (SingleExpr l)) -> Query i l (t (SingleExpr l))
children q =
  do par <- q
     chd <- table
     let (Conj from to) = parentJoin
     restrict (par |.| from .==. chd |.| to)
     return chd

infixl 5 *->
infixl 3 *+>

child :: ChildSelector t => Query i l (ParentTable t (SingleExpr l)) -> RecordKey t -> Query i l (t (SingleExpr l))
child q k = sieve (\tab -> tab |.| keyField .==. constant k) (children q)

(*->) :: ChildSelector t => Query i l (ParentTable t (SingleExpr l)) -> RecordKey t -> Query i l (t (SingleExpr l))
(*->) = child

withParent :: (MeetAggr Single i ~ i, MeetType Value r ~ r, ChildSelector t)
           => Query i l (ParentTable t (SingleExpr l)) -> Query i l (t (Exp r i l)) -> Query i l (t (Exp r i l))
withParent p q =
  do pr <- p
     qr <- q
     let (Conj from to) = parentJoin
     return $ to |->| pr |.| from $ qr

(*+>) :: (MeetAggr Single i ~ i, MeetType Value r ~ r, ChildSelector t)
      => Query i l (ParentTable t (SingleExpr l)) -> Query i l (t (Exp r i l)) -> Query i l (t (Exp r i l))
(*+>) = withParent


-- | Type-class trick to do child-selection as a multivariate funtion.

record :: forall a. (SelectAggr' a ~ Single, PrimarySelector (SelectTable a), SelectRecord a) => RecordKey (SelectTable a) -> a
record v = selectRecord' $ tableSieve $ \tab -> con v .==. tab |.| keyField

type family SelectSingle (a :: *) :: Constraint
type instance SelectSingle (a -> r) = SelectSingle r
type instance SelectSingle (Query i l (t (Expr i' l))) = i' ~ Single

class SelectSingle a => SelectRecord a where
  type SelectTable  a :: (* -> *) -> *
  type SelectAggr   a :: *
  type SelectLevel  a :: *
  type SelectAggr'  a :: *
  selectRecord' :: Query (SelectAggr a) (SelectLevel a) (SelectTable a (Expr (SelectAggr' a) (SelectLevel a))) -> a

instance i' ~ Single => SelectRecord (Query i l (t (Expr i' l))) where
  type SelectTable (Query i l (t (Expr i' l))) = t
  type SelectAggr  (Query i l (t (Expr i' l))) = i
  type SelectLevel (Query i l (t (Expr i' l))) = l
  type SelectAggr' (Query i l (t (Expr i' l))) = i'
  selectRecord' = id

instance (SelectAggr' r ~ Single, SelectRecord r, ChildSelector (SelectTable r), a ~ RecordKey (SelectTable r)) => SelectRecord (a -> r) where
  type SelectTable (a -> r) = ParentTable (SelectTable r)
  type SelectAggr  (a -> r) = SelectAggr  r
  type SelectLevel (a -> r) = SelectLevel r
  type SelectAggr' (a -> r) = SelectAggr' r
  selectRecord' q v =
    let (Conj from to) = parentJoin
    in selectRecord' $
          do par <- q
             chd <- tableSieve (\tab -> con v .==. tab |.| keyField)
             restrict $ par |.| from .==. chd |.| to
             return chd