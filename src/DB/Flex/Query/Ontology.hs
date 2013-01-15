{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts
           , TypeOperators, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances
           , Rank2Types #-}
module DB.Flex.Query.Ontology where

import Data.Convertible
import Data.Label
import Data.Proxy
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Typed

data OneToOne     = OneToOne
data OneToZeroOne = OneToZeroOne
data OneToMany    = OneToMany

type a |> b = forall f. a f :-> f b
data (><) a b x = Conj (a |> x) (b |> x)

(>-<) :: (a |> x) -> (b |> x) -> (a >< b) x
(>-<) = Conj


class (DBTable t, Ord (PrimKey t), Convertible (PrimKey t) SqlValue) => DBPrimary t where
  type PrimKey t :: *
  primKey :: t f :-> f (PrimKey t)

class (DBTable t, DBPrimary t') => Foreign t t' where
--  type Relation t t' :: *
  refKey :: t' f -> t f :-> f (PrimKey t')

refKey' :: forall t t' f. Foreign t t' => Proxy (t' f) -> t f :-> f (PrimKey t')
refKey' Proxy = refKey (undefined :: t' f)

{-
parent :: (Foreign t t', Eq (PrimKey t')) => Query i l (t (SingleExpr l)) -> Query i l (t' (SingleExpr l))
parent q = 
  do par   <- table
     child <- q
     restrict (par |.| primKey .==. child |.| refKey par)
     return par

children :: (Foreign t t', Eq (PrimKey t')) => Query i l (t' (SingleExpr l)) -> Query i l (t (SingleExpr l))
children q = 
  do par <- q
     child <- table
     restrict (par |.| primKey .==. child |.| refKey par)
     return child
-}

class Queryable a where
  type QueryTable a :: (* -> *) -> *
  filterQuery :: a -> QueryTable a (SingleExpr l) -> Query i l (QueryTable a (SingleExpr l))

fieldFilter :: (DBTable t, Eq x, Convertible x SqlValue) 
            => a :-> x -> t |> x -> a 
            -> t (SingleExpr l) -> Query i l (t (SingleExpr l))
fieldFilter vField dField v = sieve (\tab -> tab |.| dField .==. constant (v |.| vField)) . return
