{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts
           , TypeOperators, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances
           , Rank2Types, FunctionalDependencies, UndecidableInstances
           , ConstraintKinds, GADTs #-}
module DB.Flex.Query.Ancestry where

import Control.Monad
import Data.Convertible
import Data.Label.Util
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Ontology
import DB.Flex.Query.Typed
import GHC.Prim

-- | A type class representing a representative column for a table
class (Table t, Eq (RecordKey t), Convertible (RecordKey t) SqlValue) => RecordSelector t where
  type RecordKey t :: *
  keyField :: t :> RecordKey t

-- | For tables containing unique rows
class RecordSelector t => PrimarySelector t

-- | Selector whose uniqueness is tied to an external table
class (RecordSelector t, RecordSelector (ParentTable t), Foreign t (ParentTable t)) => ChildSelector t where
  type ParentTable t :: (* -> *) -> *

-- | Select a row from a table
selector :: PrimarySelector t => RecordKey t -> Query i l (t (SingleExpr l))
selector v = tableSieve $ \tab -> con v .==. tab |.| keyField

-- | A version of children with restricted ancestry types
selectorChildren :: ChildSelector t => ParentTable t (SingleExpr l) -> Query i l (t (SingleExpr l))
selectorChildren = children

infixl 5 *->
infixr 3 *+>

-- | Select the child of a table
child :: ChildSelector t => RecordKey t -> ParentTable t (SingleExpr l) -> Query i l (t (SingleExpr l))
child k = sieve (\tab -> tab |.| keyField .==. constant k) <=< children


(*->) :: ChildSelector t => Query i l (ParentTable t (SingleExpr l)) -> RecordKey t -> Query i l (t (SingleExpr l))
q *-> k = q >>= child k

-- | Add another query as parent
withParent :: (MeetType Value r ~ r', ChildSelector t)
           => Query i l (ParentTable t (SingleExpr l)) -> t (Exp r Single l) -> Query i l (t (Exp r' Single l))
withParent p qr =
  do pr <- p
     let keys = foreignKey
     return $ firstJoin keys |->| pr |.| secondJoin keys $ qr

(*+>) :: (MeetType Value r ~ r', ChildSelector t)
      => Query i l (ParentTable t (SingleExpr l)) -> Query i l (t (Exp r Single l)) -> Query i l (t (Exp r' Single l))
q *+> q' = q' >>= withParent q


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
    let keys = foreignKey
    in selectRecord' $
          do par <- q
             chd <- tableSieve (\tab -> con v .==. tab |.| keyField)
             restrict $ par |.| secondJoin keys .==. chd |.| firstJoin keys
             return chd