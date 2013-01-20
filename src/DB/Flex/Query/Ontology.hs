{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts
           , TypeOperators, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances
           , Rank2Types #-}
module DB.Flex.Query.Ontology where

import Data.Convertible
import Data.Label.Util
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Typed

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
  foreignKeys :: (t :><: t') (ForeignKey t t')

parent :: Foreign t t' => Query i l (t (SingleExpr l)) -> Query i l (t' (SingleExpr l))
parent q = 
  do par   <- table
     child <- q
     let keys = foreignKeys
     restrict (par |.| secondJoin keys .==. child |.| firstJoin keys)
     return par

children :: Foreign t t' => Query i l (t' (SingleExpr l)) -> Query i l (t (SingleExpr l))
children q = 
  do par <- q
     child <- table
     let keys = foreignKeys
     restrict (par |.| secondJoin keys .==. child |.| firstJoin keys)
     return child
