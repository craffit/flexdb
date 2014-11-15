{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators, KindSignatures
           , OverlappingInstances, ScopedTypeVariables, FlexibleInstances, FlexibleContexts
           , UndecidableInstances, TypeSynonymInstances, TupleSections, TypeFamilies
           , TemplateHaskell, EmptyDataDecls, Rank2Types #-}
module DB.Flex.Query.Typed
   ( query, insert, insertMany, update, delete
   
   , restrict, having, sieve, tableSieve, unique, forUpdate, forShare
   
   , table, values, singleValues, select
   
   , union, unionAll, intersect, except
   
   , group, groupAll, scope, exists, _in, _in', like, cat, notin
   
   , asc, desc, order, limit, offset
   
   , now, timeofday, dbTrue, dbFalse, specInsert
   
   , constant, con, con', isNull, notNull, _default, toAbstractInsert, defaultInsert, ignore, emptyUpdate, isDefault, isIgnore
   
   , count, avg, stddev, variance, _sum, _max, _min, _and, _or
   
   , (.*.), (./.), (.%.), (.+.), (.-.), (.++.)
   , (.==.), (.<>.), (.<.), (.<=.), (.>.), (.>=.), (.&&.), (.||.)

   , (|.|), (|->|), (|->>|), cast
   
   , query', insert', insertMany', update', delete'
   
   , Query, Exp, Expr, InsertExpr, SortExpr, UpdateExpr, ConstantExpr, SingleExpr, AggrExpr
   
   , Insert, Value, Sort, Update, Constant, Single, Aggr, Z, Sub
   
   , MeetAggr, MeetType
   
   ) where

import Data.Functor1
import Data.Foldable1
import Data.Traversable1
import Data.Zippable1
import Data.Record.Abstract

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State hiding (modify, get)
import Data.Convertible
import Data.Label
import Data.List hiding (union, intersect, group, delete, insert)
import Data.Time.Clock
import Data.Tuple
import Database.HDBC
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Base (BaseExpr, tables, (<->), list, parens, renderQuery)
import DB.Flex.Query.Core

import qualified DB.Flex.Query.Base as B

cast :: (MeetType t t' ~ t', MeetAggr i i' ~ i') => Exp t i l a -> Exp t' i' l a
cast = castExp

-- | Restricts the records to only those who evaluates the
-- expression to True.
restrict :: SingleExpr l Bool -> Query i l ()
restrict (Exp e) = queryExp e >>= updateBaseQuery . modify B.restrict . B.binOp "AND"

-- | Having only works on aggregated expessions
having :: AggrExpr l Bool -> Query Aggr l ()
having (Exp e) = queryExp e >>= updateBaseQuery . modify B.having . B.binOp "AND"

-- | Data-base version of filter
sieve :: (t (SingleExpr l) -> SingleExpr l Bool) -> t (SingleExpr l) -> Query i l (t (SingleExpr l))
sieve p v = restrict (p v) >> return v

-- | Fore unique query
unique :: Query i l ()
unique = updateBaseQuery $ set B.unique True

-- | Select query 'for update', does not work on aggreagted expressions
forUpdate :: Query Single l ()
forUpdate = updateBaseQuery $ set B.for (Just "update")

-- | Select query 'for share' locking
forShare :: Query Single l ()
forShare = updateBaseQuery $ set B.for (Just "share")


-- | Return all records from a specific table.
projectQuery :: Record r => BaseExpr String -> r FieldName -> Query i l (r (SingleExpr l))
projectQuery tb nms =
   do a <- newAlias
      let tName  = "table" ++ show a
          vNames = collect unFieldName nms
      updateBaseQuery $ modify tables ((tb <-> return ("as " ++ tName ++ " (" ++ intercalate ", " vNames ++ ")")) :)
      return $ fmap1 (Exp . return . return . ((tName ++ ".") ++) . unFieldName) nms

projectQuerySimple :: Record r => BaseExpr String -> r FieldName -> Query i l (r (SingleExpr l))
projectQuerySimple tb nms =
   do a <- newAlias
      let tName  = "table" ++ show a
      updateBaseQuery $ modify tables ((tb <-> return ("as " ++ tName)):)
      return $ fmap1 (Exp . return . return . ((tName ++ ".") ++) . unFieldName) nms

-- | Project a table
table :: forall t i l. Table t => Query i l (t (SingleExpr l))
table = let tName  = tableName (undefined :: t (SingleExpr l))
        in projectQuerySimple (return tName) fieldNames

tableSieve :: Table t => (t (SingleExpr l) -> SingleExpr l Bool) -> Query i l (t (SingleExpr l))
tableSieve p = table >>= sieve p

fieldList :: Record r => r a -> r FieldName
fieldList r = distribute (\_ v -> FieldName $ "v" ++ show v) [(1 :: Int)..] r

-- | Project a list of values (NOTE: this may lead to postgre type-inference problems)
values :: (Record r, AbstractType t r) => [t] -> Query i l (r (SingleExpr l))
values [] = error "Cannot construct values for an empty list"
values dat =
   let vs = parens $ return "values" <-> fmap (intercalate ", ") (mapM (list . mapM B.value . recordValues . toAbstract) dat)
   in projectQuery vs (fieldList $ toAbstract $ head dat)

-- | Insert a list of singular values
singleValues :: ( AbstractType (AbstractVal x Identity) (AbstractVal x), DBType x)
             => [x] -> Query Single l (SingleExpr l x)
singleValues = fmap (get realVal) . values . map (AbstractVal . Identity)

renderSubQuery :: Record r => Query i (Sub j l) (r (Expr i (Sub j l))) -> Query i' l (r (Expr i (Sub j l)), BaseExpr String)
renderSubQuery q =
  do (rec, bq) <- runSubQuery q
     exps <- mapM queryExp (collect bExp rec)
     return (rec, renderQuery bq (sequence exps))

renderSubExp :: Record r => Query i (Sub j l) (r (Expr i (Sub j l))) -> State Int (r (Expr i (Sub j l)), BaseExpr String)
renderSubExp q =
  do (rec, bq) <- expQuery q
     exps <- sequence (collect bExp rec)
     return (rec, renderQuery bq (sequence exps))

select :: Record r => Query i (Sub i' l) (r (Expr i (Sub i' l))) -> Query i' l (r (SingleExpr l))
select q = renderSubQuery q >>= \(rec,be) -> projectQuery be (fieldList rec)

binQ :: Record r => String 
                  -> Query i (Sub i'' l) (r (Expr i (Sub i'' l))) 
                  -> Query i' (Sub i'' l) (r (Expr i' (Sub i'' l))) 
                  -> Query i'' l (r (SingleExpr l))
binQ op q1 q2 =
  do (rec, be1) <- renderSubQuery q1
     (_, be2)   <- renderSubQuery q2
     projectQuery (parens $ parens be1 <-> return op <-> parens be2) (fieldList rec)

union :: Record r
      => Query i (Sub i'' l) (r (Expr i (Sub i'' l))) 
      -> Query i' (Sub i'' l) (r (Expr i' (Sub i'' l))) 
      -> Query i'' l (r (SingleExpr l))
union = binQ "union"

unionAll :: Record r
      => Query i (Sub i'' l) (r (Expr i (Sub i'' l))) 
      -> Query i' (Sub i'' l) (r (Expr i' (Sub i'' l))) 
      -> Query i'' l (r (SingleExpr l))
unionAll = binQ "union all"

intersect :: Record r
      => Query i (Sub i'' l) (r (Expr i (Sub i'' l))) 
      -> Query i' (Sub i'' l) (r (Expr i' (Sub i'' l))) 
      -> Query i'' l (r (SingleExpr l))
intersect = binQ "intersect"

except :: Record r
      => Query i (Sub i'' l) (r (Expr i (Sub i'' l))) 
      -> Query i' (Sub i'' l) (r (Expr i' (Sub i'' l))) 
      -> Query i'' l (r (SingleExpr l))
except = binQ "except"

scope :: Expr i l a -> ConstantExpr (Sub i l) a
scope = castExp

group :: SingleExpr l a -> Query Aggr l (AggrExpr l a)
group (Exp e) =
  do ex <- queryExp e
     updateBaseQuery $ modify B.group (++[ex])
     return (Exp e)

groupAll :: Record r => r (SingleExpr l) -> Query Aggr l (r (AggrExpr l))
groupAll = traverse1 group

-- | Basic values

now :: Expr Constant l UTCTime
now = Exp  $ return $ return "now()"

timeofday :: Expr Constant l String
timeofday = Exp $ return $ return "timeofday()"

dbTrue :: Expr Constant l Bool
dbTrue = Exp $ return $ return "true"

dbFalse :: Expr Constant l Bool
dbFalse = Exp $ return $ return "false"

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
unOp :: String -> Expr i l t -> Expr i l t'
unOp op = Exp . fmap (B.unOp op) . bExp

postOp :: String -> Expr i' l t -> Expr i' l t'
postOp op = Exp . fmap (B.postOp op) . bExp

binOp :: String -> Expr i l t -> Expr i' l t' -> Expr i'' l t''
binOp op (Exp e1) (Exp e2) = Exp $ B.binOp op <$> e1 <*> e2

class Args a where arg_ :: String -> [State Int (BaseExpr String)] -> a
instance Args tail => Args (Exp Value i l t -> tail) where arg_ name exprs = arg_ name . (:exprs) . bExp
instance Args (Exp Value i l t)                      where arg_ name exprs = Exp $ B.func name . sequence <$> sequence (reverse exprs)

func :: (Args a) => String -> a
func name = arg_ name []

infix   8 `like`, `_in`
infixl  7 .*., ./., .%.
infixl  6 .+.,.-.
infixr  5 .++.
infix   4 .==., .<>., .<., .<=., .>., .>=.
infixr  3 .&&.
infixr  2 .||.


-- | Equality comparison on Exprs, = in SQL.
(.==.) :: Eq a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l Bool
(.==.) = binOp "="

-- | Inequality on Exprs, <> in SQL.
(.<>.) :: Eq a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l Bool
(.<>.) = binOp "<>"

(.<.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l Bool
(.<.)  = binOp "<"

(.<=.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l Bool
(.<=.) = binOp "<="

(.>.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l Bool
(.>.)  = binOp ">"

(.>=.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l Bool
(.>=.) = binOp ">="

-- | The inverse of an Expr Bool.
_not :: Expr i l Bool -> Expr i l Bool
_not = unOp "NOT"

-- | \"Logical and\" on 'Expr', AND in SQL.
(.&&.) :: Expr i l Bool -> Expr i' l Bool -> Expr (MeetAggr i i') l Bool
(.&&.) = binOp "AND"

-- | \"Logical or\" on 'Expr'. OR in SQL.
(.||.) :: Expr i l Bool -> Expr i' l Bool -> Expr (MeetAggr i i') l Bool
(.||.) = binOp "OR"

like :: Expr i l String -> Expr i' l String -> Expr (MeetAggr i i') l Bool
like   = binOp "LIKE"


-- | Produces the concatenation of two String-expressions.
cat :: Expr i l String -> Expr i' l String -> Expr (MeetAggr i i') l String
cat = binOp "||"

-- | Concatenates two String-expressions.
(.++.) :: Expr i l String -> Expr i' l String -> Expr (MeetAggr i i') l String
(.++.) = cat

-- | Gets the length of a string.
_length :: Expr i l String -> Expr i l Int
_length = func "char_length"

-- | Addition
(.+.) :: Num a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l a
(.+.) = binOp "+"
-- | Subtraction
(.-.) :: Num a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l a
(.-.) = binOp "-"
-- | Multiplication
(.*.) :: Num a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l a
(.*.) = binOp "*"
-- | Division
(./.) :: Num a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l a
(./.) = binOp "/"
-- | Modulo
(.%.) :: Num a => Expr i l a -> Expr i' l a -> Expr (MeetAggr i i') l a
(.%.) = binOp "%"

-- | Returns true if the expression is Null.
isNull :: Expr i l a -> Expr i l Bool
isNull  = postOp "ISNULL"

-- | The inverse of 'isNull', returns false
-- if the expression supplied is Null.
notNull :: Expr i l a -> Expr i l Bool
notNull = postOp "NOTNULL"

exists :: Record r => Query i (Sub j l) (r (Expr i (Sub j l))) -> Expr j l Bool
exists q = unOp "exists" $ Exp $ snd <$> renderSubExp q

_in :: (Convertible a SqlValue, Eq a) => Expr j l a -> [a] -> Expr j l Bool
_in (Exp e) dat = Exp $ (\ex -> ex <-> return "in" <-> parens (fmap (intercalate ", ") (mapM (B.value . toSql) dat))) <$> e

_in' :: (Eq a, DBType a)
      => Expr j l a
      -> Query i (Sub j l) (Expr i (Sub j l) a)
      -> Expr j l Bool
_in' e q = binOp "in" e $ Exp $ fmap snd $ renderSubExp (fmap AbstractVal q)

notin :: (Eq a, DBType a)
      => Expr j l a
      -> Query i (Sub j l) (Expr i (Sub j l) a)
      -> Expr j l Bool
notin e q = binOp "not in" e $ Exp $ fmap snd $ renderSubExp (fmap AbstractVal q)

-----------------------------------------------------------
-- Default values
-----------------------------------------------------------

-- | The default value of the column. Only works with 'insert'.
_default :: InsertExpr i l a
_default = Exp $ return $ return "DEFAULT"

defaultInsert :: forall t l. Table t => t (InsertExpr Single l)
defaultInsert = fmap1 (const _default) (recordFields :: t Field)

toAbstractInsert :: (Record t, AbstractType a t) => a -> t (InsertExpr i l)
toAbstractInsert = zip1 (\Field (Identity v) -> con' v) recordFields . toAbstract

specInsert :: forall t l v. Table t => t (FieldData v) -> t (InsertExpr Single l)
specInsert = zip1 mkField recordFields
  where mkField :: Field a -> FieldData v a -> InsertExpr Single l a
        mkField Field NoData = _default
        mkField Field (GotData a) = con' a

ignore :: UpdateExpr a
ignore = Exp $ return $ return "IGNORE"

emptyUpdate :: forall t. Table t => t UpdateExpr
emptyUpdate = fmap1 (const ignore) (recordFields :: t Field)

isDefault :: InsertExpr i l a -> Bool
isDefault = expEquals "DEFAULT"

isIgnore :: UpdateExpr a -> Bool
isIgnore = expEquals "IGNORE"

-- | Creates a constant expression from a haskell value.
constant :: Convertible a SqlValue => a -> ConstantExpr l a
constant x = Exp $ return $ B.value (toSql x)

con :: Convertible a SqlValue => a -> ConstantExpr l a
con = constant

-- | A version of constant which infers its type from the surroundings
con' :: Convertible a SqlValue => a -> Exp t i l a
con' x = Exp $ return $ B.value (toSql x)

-- | Returns the number of records (=rows) in a query.
count :: SingleExpr l a -> AggrExpr l Int
count = func "count"

-- | Returns the total sum of a column.
_sum :: Num a => SingleExpr l a -> AggrExpr l a
_sum = func "sum"

-- | Returns the highest value of a column.
_max :: Ord a => SingleExpr l a -> AggrExpr l a
_max = func "max"

-- | Returns the lowest value of a column.
_min :: Ord a => SingleExpr l a -> AggrExpr l a
_min = func "min"

-- | Returns the average of a column.
avg :: Num a => SingleExpr l a -> AggrExpr l a
avg = func "avg"

-- | Returns the standard deviation of a column.
stddev :: Num a => SingleExpr l a -> AggrExpr l a
stddev = func "stddev"

-- | Returns the standard variance of a column.
variance :: Num a => SingleExpr l a -> AggrExpr l a
variance = func "variance"

_and :: SingleExpr l Bool -> AggrExpr l Bool
_and = func "bool_and"

_or :: SingleExpr l Bool -> AggrExpr l Bool
_or = func "bool_or"

-----------------------------------------------------------
-- Special ops
-----------------------------------------------------------

-- | Return the n topmost records.
limit :: Int -> Query i l ()
limit = updateBaseQuery . set B.limit . Just

offset :: Int -> Query i l ()
offset = updateBaseQuery . set B.offset . Just

-----------------------------------------------------------
-- Sorting results
-----------------------------------------------------------
data Order = ASC | DESC deriving Show

orderOp :: Ord a => Order -> Expr i l a -> SortExpr i l a
orderOp op e = Exp $  (<-> return (show op)) <$> bExp e

-- | Use this together with the function 'order' to
-- order the results of a query in ascending order.
-- Takes a relation and an attribute of that relation, which
-- is used for the ordering.
asc :: Ord a => Expr i l a -> SortExpr i l a
asc = orderOp ASC

-- | Use this together with the function 'order' to
-- order the results of a query in descending order.
-- Takes a relation and an attribute of that relation, which
-- is used for the ordering.
desc :: Ord a => Expr i l a -> SortExpr i l a
desc = orderOp DESC

-- | Sort  the results of a query.
-- Use this with the 'asc' or 'desc' functions.
order :: SortExpr i l a -> Query i l ()
order (Exp e) = queryExp e >>= updateBaseQuery . modify B.order . flip (++) . return

-- | Convenience funtion for using getters

infixl 9 |.|

(|.|) :: a -> (a :-> b) -> b
r |.| s = get s r

infix 8 |->|
-- | Convenience function for using setters: includes phantom type cast
(|->|) :: Functor1 r => (forall f g h. r (Exp f g h) :-> Exp f g h a) 
       -> Exp t i l a -> r (Exp t' i' l) -> r (Exp (MeetType t t') (MeetAggr i i') l)
l |->| v = set l (castExp v) . fmap1 castExp

infix 8 |->>|
-- | Convenience function for using setters: includes phantom type cast
(|->>|) :: (Functor1 r, Convertible a SqlValue) 
       => (forall f g h. r (Exp f g h) :-> Exp f g h a) -> a -> r (Exp t i l) -> r (Exp t i l)
l |->>| v = set l (con' v) . fmap1 castExp

query' :: (DbMonad m, Record r) => Query i Z (r (Expr i Z)) -> m [r Identity]
query' q =
  let (rec, bq) = runQuery $ q >>= mapM queryExp . collect bExp
  in map buildRecord <$> B.baseQuery bq (sequence rec)

insert' :: forall i r m. (DbMonad m, Table r) => Query i Z (r (InsertExpr i Z)) -> m [r Identity]
insert' q =
  let (rec, bq) = runQuery $ q >>= mapM queryExp . collect bExp
      nms       = names (undefined :: r a)
      tname     = tableName (undefined :: r a)
  in map buildRecord <$> B.baseInsert bq tname (zip nms rec)

insertMany' :: forall i r m. (Table r, DbMonad m) => [Query i Z (r (InsertExpr i Z))] -> m [[r Identity]]
insertMany' qs =
  let qs'   = map (\q -> fmap (zip nms) $ swap $ runQuery $ q >>= mapM queryExp . collect bExp) qs
      nms   = names (undefined :: r a)
      tname = tableName (undefined :: r a)
  in map (map buildRecord) <$> B.baseInsertMany tname qs'

update' :: forall t m. (DbMonad m, Table t) => (t (SingleExpr Z) -> Query Single Z (t UpdateExpr)) -> m [t Identity]
update' qf =
  let tname     = tableName (undefined :: t a)
      q         = qf . fmap1 (Exp . return . return . ((tname ++ ".") ++) . unFieldName) $ fieldNames
      (rec, bq) = runQuery $ q >>= mapM queryExp . collect bExp
      nms       = names (undefined :: t a)
  in map buildRecord <$> B.baseUpdate bq tname (zip nms rec)

delete' :: forall t m. (Table t, DbMonad m) => (t (SingleExpr Z) -> Query Single Z ()) -> m [t Identity]
delete' qf =
  let tName   = tableName (undefined :: t v)
      tFields = map ((tName ++ ".")++) $ names (undefined :: t v)
      (_, bq) = runQuery . qf . fmap1 (Exp . return . return . ((tName ++ ".") ++) . unFieldName) $ fieldNames
  in map buildRecord <$> B.baseDelete bq tName tFields

query :: (Record r, AbstractType a r, DbMonad m) => Query i Z (r (Expr i Z)) -> m [a]
query = fmap (map fromAbstract) . query'

insert :: (Table r,  AbstractType a r, MeetType t Insert ~ Insert, DbMonad m) 
       => Query i Z (r (Exp t i l)) -> m [a]
insert = fmap (map fromAbstract) . insert' . fmap (fmap1 castExp)

insertMany :: (Table r,  AbstractType a r, MeetType t Insert ~ Insert, DbMonad m) 
           => [Query i Z (r (Exp t i l))] -> m [[a]]
insertMany = fmap (map $ map fromAbstract) . insertMany' . map (fmap (fmap1 castExp))

update :: (Table r, AbstractType a r, MeetType t Update ~ Update, DbMonad m) 
       => (r (SingleExpr Z) -> Query Single Z (r (Exp t Single Z))) -> m [a]
update = fmap (map fromAbstract) . update' . fmap (fmap $ fmap1 castExp)

delete :: (Table r, AbstractType a r, DbMonad m) => (r (SingleExpr Z) -> Query Single Z ()) -> m [a]
delete = fmap (map fromAbstract) . delete'
