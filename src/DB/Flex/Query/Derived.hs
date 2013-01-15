{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FunctionalDependencies, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module DB.Flex.Query.Derived where

{-
import Data.Proxy

import DB.Flex.Record
import DB.Flex.Query.Typed

class (DBTable t, DBTable t2, Eq (KeyType t t2)) => DBForeign t t2 where
  type KeyType t t2 :: *
  fromKey    :: Proxy (t2 a) -> t f -> f (KeyType t t2)
  setFromKey :: Proxy (t2 a) -> f (KeyType t t2) -> t f -> t f
  toKey      :: Proxy (t a) -> t2 f -> f (KeyType t t2)
  setToKey   :: Proxy (t a) -> f (KeyType t t2) -> t2 f -> t2 f

foreign :: forall t t2. DBForeign t t2 => t Expr -> Query (t2 Expr)
foreign par =
  do child <- table
     restrict $ fromKey (Proxy :: Proxy (t2 a)) par .==. toKey (Proxy :: Proxy (t a)) child
     return child

class DBForeign (DBParent t) t => DBChild t where
  type DBParent t :: (* -> *) -> *

children :: forall t. DBChild t => DBParent t Expr -> Query (t Expr)
children = foreign

insertChildren :: forall t. DBChild t => Query (DBParent t Expr) -> Query (t InsertExpr) -> Query (t InsertExpr)
insertChildren p i =
  do par <- p
     chd <- i
     return $ setToKey (Proxy :: Proxy (DBParent t a)) (insertExpr $ fromKey (Proxy :: Proxy (t a)) par) chd

class DBTable (SelectTable t) => DBSelect t where
  type SelectTable t :: (* -> *) -> *
  select :: t -> Query (SelectTable t Expr)

selectChildren :: (DBSelect t, DBChild c, DBParent c ~ SelectTable t) => t -> Query (c Expr)
selectChildren t = select t >>= children

deleteChildren :: forall t t2. DBForeign t t2 => (t Expr -> Query ()) -> (t2 Expr -> Query ())
deleteChildren fparq child =
  do par <- (table :: Query (t Expr))
     fparq par
     restrict $ fromKey (Proxy :: Proxy (t2 a)) par .==. toKey (Proxy :: Proxy (t a)) child
-}
-- childOf :: forall t t2 v. DBForeign t t2 v => t Expr -> t2 InsertExpr -> Query (t2 InsertExpr)