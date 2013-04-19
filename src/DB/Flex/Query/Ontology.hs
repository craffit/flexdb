{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts
           , TypeOperators, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances
           , Rank2Types #-}
module DB.Flex.Query.Ontology where

import Data.Convertible
import Data.Label.Util
import DB.Flex.Record
import DB.Flex.Query.Typed
import Database.HDBC

{-
data OneToOne     = OneToOne
data OneToZeroOne = OneToZeroOne
data OneToMany    = OneToMany

type family Multiplicity a b :: *
type instance Multiplicity OneToOne a     = a
type instance Multiplicity OneToZeroOne a = Maybe a
type instance Multiplicity OneToMany a    = [a]
-}

-- | Class representing the primary key of a database table
class (Table t, Convertible (PrimKey t) SqlValue) => Primary t where
  type PrimKey t :: *
  primKey :: t :> (PrimKey t)

-- | Class representing foriegn key relationships between tables
class (Table t, Table t', Eq (ForeignKey t t')) => Foreign t t' where
--  type Relation t t' :: *
  type ForeignKey t t' :: *
  foreignKey :: (t :><: t') (ForeignKey t t')

parent :: Foreign t t' => t (SingleExpr l) -> Query i l (t' (SingleExpr l))
parent = join $ flipJoin foreignKey

children :: Foreign t t' => t' (SingleExpr l) -> Query i l (t (SingleExpr l))
children = join foreignKey

join :: (Table t, Eq x) => (t :><: t') x -> t' (SingleExpr l) -> Query i l (t (SingleExpr l))
join keys par = tableSieve $ \tab -> par |.| secondJoin keys .==. tab |.| firstJoin keys

infixl 5 >*<

(>*<) :: (Table t, Eq x) => t :> x -> t' :> x -> t' (SingleExpr l) -> Query i l (t (SingleExpr l))
k1 >*< k2 = join $ k1 >-< k2
