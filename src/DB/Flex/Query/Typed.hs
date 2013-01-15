{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators, KindSignatures
           , OverlappingInstances, ScopedTypeVariables, FlexibleInstances, FlexibleContexts
           , UndecidableInstances, TypeSynonymInstances, TupleSections, DataKinds, PolyKinds, TypeFamilies
           , TemplateHaskell, EmptyDataDecls, Rank2Types #-}
module DB.Flex.Query.Typed
   ( query, insert, update, delete
   
   , restrict, having, sieve, tableSieve, unique, forUpdate, forShare
   
   , table, values, singleValues, select
   
   , union, unionAll, intersect, except
   
   , group, groupAll, scope, exists, _in, _in', like, cat, notin
   
   , asc, desc, order, limit, offset
   
   , constant, con, isNull, notNull, _default, defaultInsert, ignore, emptyUpdate, isDefault, isIgnore
   
   , count, avg, stddev, variance, _sum, _max, _min, _and, _or
   
   , (.*.), (./.), (.%.), (.+.), (.-.), (.++.)
   , (.==.), (.<>.), (.<.), (.<=.), (.>.), (.>=.), (.&&.), (.||.)

   , (|.|), (|->|)
   
   , query', insert', update', delete'
   
   , Query, Exp, Expr, InsertExpr, SortExpr, UpdateExpr, ConstantExpr, SingleExpr, AggrExpr     
   , VType(..), VAggr(..), SubAggr(..), L(..), AggrVal, IAggr, MeetAggr, MeetType
   
   ) where

import Data.Proxy
import Data.UUID
import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State hiding (modify, get)
import qualified Control.Monad.State as ST
import Data.Convertible
import Data.Label
import Data.List hiding (union, intersect, group, delete, insert)
import Database.HDBC
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Base (BaseQuery(..), BaseExpr, tables, (<->), list, parens, renderQuery)
import qualified DB.Flex.Query.Base as B
import Safe

instance Convertible UUID SqlValue where
  safeConvert = safeConvert . show

instance Convertible SqlValue UUID where
  safeConvert sql =
    do str <- safeConvert sql
       let uid = readMay str
       maybe (convError "Error converting String to UUID in Data.UUID.Instances." sql) return uid


-- | Expression types
data VType = Insert | Value | Sort | Update

infix 5 \/

type family (\/) (a :: VType) (b :: VType) :: VType
type instance (\/) Insert Insert = Insert
type instance (\/) Value  Value  = Value
type instance (\/) Sort   Sort   = Sort
type instance (\/) Update Update = Update
type instance (\/) Value  Insert = Insert
type instance (\/) Value  Sort   = Sort
type instance (\/) Value  Update = Update
type instance (\/) Insert Value  = Insert
type instance (\/) Sort   Value  = Sort
type instance (\/) Update Value  = Update
type instance (\/) Insert Update = Update
type instance (\/) Update Insert = Update

type family MeetType (a :: VType) (b :: VType) :: VType
type instance MeetType a b = a \/ b

-- | Aggregation state
data VAggr   = Constant | Single | Aggr
data SubAggr = SubV VAggr | None
data L       = Sub SubAggr L | Z

infix 5 \|/

type family (\|/) (a :: VAggr) (b :: VAggr) :: VAggr
type instance (\|/) Constant Constant = Constant
type instance (\|/) Constant Single   = Single
type instance (\|/) Single   Constant = Single
type instance (\|/) Single   Single   = Single
type instance (\|/) Constant Aggr     = Aggr
type instance (\|/) Aggr     Constant = Aggr
type instance (\|/) Aggr     Aggr     = Aggr

type family MeetAggr (a :: VAggr) (b :: VAggr) :: VAggr
type instance MeetAggr a b = a \|/ b

data ConstantI
data SingleI
data AggrI

type family IAggr (v :: VAggr) :: *
type instance IAggr Constant = ConstantI
type instance IAggr Single   = SingleI
type instance IAggr Aggr     = AggrI

class AggrVal v where isAggr :: Proxy v -> Bool
instance AggrVal ConstantI where isAggr _ = False
instance AggrVal SingleI   where isAggr _ = False
instance AggrVal AggrI     where isAggr _ = True

newtype Exp (a :: VType) (i :: VAggr) (l :: L) t = Exp { _bExp :: State Int (BaseExpr String) }

$( mkLabels [''Exp] )

-- Haskell-level function!
expEquals :: String -> Exp x i l a -> Bool
expEquals s = (== s) . fst . flip runState [] . fst . flip runState 0 . get bExp

type Expr         = Exp Value
type InsertExpr i = Exp Insert i Z
type SortExpr     = Exp Sort
type UpdateExpr   = Exp Update Single Z

type ConstantExpr = Exp Value Constant
type SingleExpr   = Exp Value Single
type AggrExpr     = Exp Value Aggr

type Alias      = Int
data QState (t :: VAggr) (l :: L)
                 = QState { _alias :: Alias
                          , _build :: BaseQuery
                          }

$( mkLabels [''QState] )

type Query (t :: VAggr) (l :: L) a    = State (QState t l) a

castExp :: Exp v i l a -> Exp v' i' l' a'
castExp = Exp . get bExp

{-
castQState :: QState t l -> QState t' l'
castQState (QState a b) = QState a b

castQuery :: Query t l a -> Query t' l' a
castQuery q = StateT $ fmap (second castQState) . runStateT q . castQState
-}

newAlias :: Query t l Int
newAlias = ST.modify (modify alias (+1)) >> gets (get alias)

updateBaseQuery :: (BaseQuery -> BaseQuery) -> Query t l ()
updateBaseQuery = ST.modify . modify build

runExp :: State Int (BaseExpr String) -> Query i' l' (BaseExpr String)
runExp e =
  do st <- ST.get
     let (ex, a) = runState e (get alias st)
     put (set alias a st)
     return ex


runQueryAlias :: Int -> Query i l a -> (a, QState i l)
runQueryAlias a = flip runState (QState a $ BaseQuery [] (return "true") (return "true") Nothing Nothing False [] [] Nothing)

runQuery :: Query i l a -> (a, BaseQuery)
runQuery = second (get build) . runQueryAlias 0

runSubQuery :: Query i l a -> Query i' l' (a, BaseQuery)
runSubQuery q =
  do QState a bq <- ST.get
     let (r, QState a' bq') = runQueryAlias a q
     put (QState a' bq)
     return (r, bq')

runExpQuery :: Query i' l' a -> State Int (a, BaseQuery)
runExpQuery q =
  do a <- ST.get
     let (r, QState a' bq) = runQueryAlias a q
     put a'
     return (r, bq)

-- | Restricts the records to only those who evaluates the
-- expression to True.
restrict :: SingleExpr l Bool -> Query i l ()
restrict (Exp e) = runExp e >>= updateBaseQuery . modify B.restrict . B.binOp "AND"

having :: AggrExpr l Bool -> Query Aggr l ()
having (Exp e) = runExp e >>= updateBaseQuery . modify B.having . B.binOp "AND"

sieve :: (t (SingleExpr l) -> SingleExpr l Bool) -> Query i l (t (SingleExpr l)) -> Query i l (t (SingleExpr l))
sieve p v =
  do x <- v
     restrict $ p x
     return x

unique :: Query i l ()
unique = updateBaseQuery $ set B.unique True

forUpdate :: Query Single l ()
forUpdate = updateBaseQuery $ set B.for (Just "update")

forShare :: Query Single l ()
forShare = updateBaseQuery $ set B.for (Just "share")


-- | Return all records from a specific table.
projectQuery :: DBRecord r => BaseExpr String -> r FieldName -> Query i l (r (SingleExpr l))
projectQuery tb nms =
   do a <- newAlias
      let tName  = "table" ++ show a
          vNames = collect unFieldName nms
      updateBaseQuery $ modify tables ((tb <-> return ("as " ++ tName ++ " (" ++ intercalate ", " vNames ++ ")")) :)
      return $ fmap1 (Exp . return . return . ((tName ++ ".") ++) . unFieldName) nms

-- | Project a table
table :: forall t i l. DBTable t => Query i l (t (SingleExpr l))
table = let tName  = tableName (undefined :: t (SingleExpr l))
        in projectQuery (return tName) fieldNames

tableSieve :: DBTable t => (t (SingleExpr l) -> SingleExpr l Bool) -> Query i l (t (SingleExpr l))
tableSieve p = sieve p table

fieldList :: DBRecord r => r a -> r FieldName
fieldList r = distribute (\_ v -> FieldName $ "v" ++ show v) [(1 :: Int)..] r

-- | Project a list of values
values :: (DBRecord r, AbstractType t r) => [t] -> Query i l (r (SingleExpr l))
values [] = error "Cannot construct values for an empty list"
values dat =
   let vs = parens $ return "values" <-> fmap (intercalate ", ") (mapM (list . mapM B.value . recordValues . toAbstract) dat)
   in projectQuery vs (fieldList $ toAbstract $ head dat)

singleValues :: ( AbstractType (AbstractVal x Identity) (AbstractVal x)
                , Convertible SqlValue x, Convertible x SqlValue)
             => [x] -> Query Single l (SingleExpr l x)
singleValues = fmap (get realVal) . values . map (AbstractVal . Identity)

renderSubQuery :: forall i j r l i'. (AggrVal (IAggr i), DBRecord r) 
               => Query i (Sub j l) (r (Expr i (Sub j l))) -> Query i' l (r (Expr i (Sub j l)), BaseExpr String)
renderSubQuery q =
  do (rec, bq) <- runSubQuery q
     exps <- mapM runExp (collect (get bExp) rec)
     let aggr = isAggr (Proxy :: Proxy (IAggr i))
         be' = renderQuery aggr bq (sequence exps)
     return (rec, be')

renderSubExp :: forall i j r l. (DBRecord r, AggrVal (IAggr i)) 
      => Query i (Sub (SubV j) l) (r (Expr i (Sub (SubV j) l)))
      -> State Int (r (Expr i (Sub (SubV j) l)), BaseExpr String)
renderSubExp q =
  do (rec, bq) <- runExpQuery q
     exps <- sequence (collect (get bExp) rec)
     let aggr = isAggr (Proxy :: Proxy (IAggr i))
         be' = renderQuery aggr bq (sequence exps)
     return (rec, be')

select :: forall i r l i'. (AggrVal (IAggr i), DBRecord r) => Query i (Sub None l) (r (Expr i (Sub None l))) -> Query i' l (r (SingleExpr l))
select q = renderSubQuery q >>= \(rec,be) -> projectQuery be (fieldList rec)

binQ :: (AggrVal (IAggr i), AggrVal (IAggr i'), DBRecord r) 
      => String -> Query i (Sub None l) (r (Expr i (Sub None l))) 
                -> Query i' (Sub None l) (r (Expr i' (Sub None l))) 
                -> Query i'' l (r (SingleExpr l))
binQ op q1 q2 =
  do (rec, be1) <- renderSubQuery q1
     (_, be2)   <- renderSubQuery q2
     projectQuery (parens $ parens be1 <-> return op <-> parens be2) (fieldList rec)

union :: (AggrVal (IAggr i), AggrVal (IAggr i'), DBRecord r) 
      => Query i (Sub None l) (r (Expr i (Sub None l))) 
      -> Query i' (Sub None l) (r (Expr i' (Sub None l))) 
      -> Query i'' l (r (SingleExpr l))
union = binQ "union"

unionAll :: (AggrVal (IAggr i), AggrVal (IAggr i'), DBRecord r) 
      => Query i (Sub None l) (r (Expr i (Sub None l))) 
      -> Query i' (Sub None l) (r (Expr i' (Sub None l))) 
      -> Query i'' l (r (SingleExpr l))
unionAll = binQ "union all"

intersect :: (AggrVal (IAggr i), AggrVal (IAggr i'), DBRecord r) 
      => Query i (Sub None l) (r (Expr i (Sub None l))) 
      -> Query i' (Sub None l) (r (Expr i' (Sub None l))) 
      -> Query i'' l (r (SingleExpr l))
intersect = binQ "intersect"

except :: (AggrVal (IAggr i), AggrVal (IAggr i'), DBRecord r) 
      => Query i (Sub None l) (r (Expr i (Sub None l))) 
      -> Query i' (Sub None l) (r (Expr i' (Sub None l))) 
      -> Query i'' l (r (SingleExpr l))
except = binQ "except"

scope :: Expr i l a -> ConstantExpr (Sub (SubV i) l) a
scope = castExp

group :: SingleExpr l a -> Query Aggr l (AggrExpr l a)
group (Exp e) =
  do ex <- runExp e
     updateBaseQuery $ modify B.group (++[ex])
     return (Exp e)

groupAll :: DBRecord r => r (SingleExpr l) -> Query Aggr l (r (AggrExpr l))
groupAll = traverse1 group

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
unOp :: String -> Expr i l t -> Expr i l t'
unOp op = Exp . fmap (B.unOp op) . get bExp

postOp :: String -> Expr i' l t -> Expr i' l t'
postOp op = Exp . fmap (B.postOp op) . get bExp

binOp :: String -> Expr i l t -> Expr i' l t' -> Expr i'' l t''
binOp op (Exp e1) (Exp e2) = Exp $ B.binOp op <$> e1 <*> e2

class Args a where arg_ :: String -> [State Int (BaseExpr String)] -> a
instance Args tail => Args (Exp Value i l t -> tail) where arg_ name exprs = arg_ name . (:exprs) . get bExp
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
(.==.) :: Eq a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l Bool
(.==.) = binOp "="

-- | Inequality on Exprs, <> in SQL.
(.<>.) :: Eq a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l Bool
(.<>.) = binOp "<>"

(.<.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l Bool
(.<.)  = binOp "<"

(.<=.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l Bool
(.<=.) = binOp "<="

(.>.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l Bool
(.>.)  = binOp ">"

(.>=.) :: Ord a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l Bool
(.>=.) = binOp ">="

-- | The inverse of an Expr Bool.
_not :: Expr i l Bool -> Expr i l Bool
_not = unOp "NOT"

-- | \"Logical and\" on 'Expr', AND in SQL.
(.&&.) :: Expr i l Bool -> Expr i' l Bool -> Expr (i \|/ i') l Bool
(.&&.) = binOp "AND"

-- | \"Logical or\" on 'Expr'. OR in SQL.
(.||.) :: Expr i l Bool -> Expr i' l Bool -> Expr (i \|/ i') l Bool
(.||.) = binOp "OR"

like :: Expr i l String -> Expr i' l String -> Expr (i \|/ i') l Bool
like   = binOp "LIKE"


-- | Produces the concatenation of two String-expressions.
cat :: Expr i l String -> Expr i' l String -> Expr (i \|/ i') l String
cat = binOp "||"

-- | Concatenates two String-expressions.
(.++.) :: Expr i l String -> Expr i' l String -> Expr (i \|/ i') l String
(.++.) = cat

-- | Gets the length of a string.
_length :: Expr i l String -> Expr i l Int
_length = func "char_length"

-- | Addition
(.+.) :: Num a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l a
(.+.) = binOp "+"
-- | Subtraction
(.-.) :: Num a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l a
(.-.) = binOp "-"
-- | Multiplication
(.*.) :: Num a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l a
(.*.) = binOp "*"
-- | Division
(./.) :: Num a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l a
(./.) = binOp "/"
-- | Modulo
(.%.) :: Num a => Expr i l a -> Expr i' l a -> Expr (i \|/ i') l a
(.%.) = binOp "%"

-- | Returns true if the expression is Null.
isNull :: Expr i l a -> Expr i l Bool
isNull  = postOp "ISNULL"

-- | The inverse of 'isNull', returns false
-- if the expression supplied is Null.
notNull :: Expr i l a -> Expr i l Bool
notNull = postOp "NOTNULL"

exists :: (DBRecord r, AggrVal (IAggr i)) 
      => Query i (Sub (SubV j) l) (r (Expr i (Sub (SubV j) l)))
      -> Expr j l Bool
exists q = unOp "exists" $ Exp $ snd <$> renderSubExp q

_in :: (Convertible a SqlValue, Eq a) => Expr j l a -> [a] -> Expr j l Bool
_in (Exp e) dat = Exp $ (\ex -> ex <-> return "in" <-> parens (fmap (intercalate ", ") (mapM (B.value . toSql) dat))) <$> e

_in' :: (AggrVal (IAggr i), Eq a, Convertible SqlValue a, Convertible a SqlValue)
      => Expr j l a
      -> Query i (Sub (SubV j) l) (Expr i (Sub (SubV j) l) a)
      -> Expr j l Bool
_in' e q = binOp "in" e $ Exp $ fmap snd $ renderSubExp (fmap AbstractVal q)

notin :: (AggrVal (IAggr i), Eq a, Convertible SqlValue a, Convertible a SqlValue)
      => Expr j l a
      -> Query i (Sub (SubV j) l) (Expr i (Sub (SubV j) l) a)
      -> Expr j l Bool
notin e q = binOp "not in" e $ Exp $ fmap snd $ renderSubExp (fmap AbstractVal q)

-----------------------------------------------------------
-- Default values
-----------------------------------------------------------

-- | The default value of the column. Only works with 'insert'.
_default :: InsertExpr i a
_default = Exp $ return $ return "DEFAULT"

defaultInsert :: forall t . DBTable t => t (InsertExpr Single)
defaultInsert = fmap1 (const _default) (recordFields :: t Field)

ignore :: UpdateExpr a
ignore = Exp $ return $ return "IGNORE"

emptyUpdate :: forall t. DBTable t => t UpdateExpr
emptyUpdate = fmap1 (const ignore) (recordFields :: t Field)

isDefault :: InsertExpr i a -> Bool
isDefault = expEquals "DEFAULT"

isIgnore :: UpdateExpr a -> Bool
isIgnore = expEquals "IGNORE"

-- | Creates a constant expression from a haskell value.
constant :: Convertible a SqlValue => a -> ConstantExpr l a
constant x = Exp $ return $ B.value (toSql x)

con :: Convertible a SqlValue => a -> ConstantExpr l a
con = constant

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
orderOp op e = Exp $  (<-> return (show op)) <$> get bExp e

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
order (Exp e) = runExp e >>= updateBaseQuery . modify B.order . flip (++) . return

{-
class Castable' (t :: * -> *) (t' :: * -> *) where
  doCast' :: t v -> t' v

instance Castable' Expr        ProjectExpr where doCast' = doCast
instance Castable' ProjectExpr ProjectExpr where doCast' = doCast
instance Castable' Expr        InsertExpr  where doCast' = doCast
instance Castable' InsertExpr  InsertExpr  where doCast' = doCast
instance Castable' Expr        UpdateExpr  where doCast' = doCast
instance Castable' UpdateExpr  UpdateExpr  where doCast' = doCast
instance Castable' InsertExpr  UpdateExpr  where doCast' = doCast

castAll :: (Functor1 r, Castable' e f) => r e -> r f
castAll = fmap1 doCast'
-}

infixl 9 |.|

(|.|) :: a -> (a :-> b) -> b
r |.| s = get s r

infix 8 |->|
(|->|) :: Functor1 r => (forall f g h. r (Exp f g h) :-> Exp f g h a) 
       -> Exp t i l a -> r (Exp t' i' l) -> r (Exp (t \/ t') (i \|/ i') l)
l |->| v = set l (castExp v) . fmap1 castExp

query' :: forall r i. (DBRecord r, AggrVal (IAggr i)) => Query i Z (r (Expr i Z)) -> Db [r Identity]
query' q =
  let (rec, bq) = runQuery $ q >>= mapM runExp . collect (get bExp)
      aggr      = isAggr (Proxy :: Proxy (IAggr i))
  in map buildRecord <$> B.baseQuery aggr bq (sequence rec)

insert' :: forall r i. (DBTable r, AggrVal (IAggr i)) => Query i Z (r (InsertExpr i)) -> Db [r Identity]
insert' q =
  let (rec, bq) = runQuery $ q >>= mapM runExp . collect (get bExp)
      aggr      = isAggr (Proxy :: Proxy (IAggr i))
      nms       = names (undefined :: r a)
      tname     = tableName (undefined :: r a)
  in map buildRecord <$> B.baseInsert aggr bq tname (zip nms rec)

update' :: forall t. DBTable t => (t (SingleExpr Z) -> Query Single Z (t UpdateExpr)) -> Db [t Identity]
update' qf =
  let tname     = tableName (undefined :: t a)
      q         = qf . fmap1 (Exp . return . return . ((tname ++ ".") ++) . unFieldName) $ fieldNames
      (rec, bq) = runQuery $ q >>= mapM runExp . collect (get bExp)
      nms       = names (undefined :: t a)
  in map buildRecord <$> B.baseUpdate bq tname (zip nms rec)

delete' :: forall t. DBTable t => (t (SingleExpr Z) -> Query Single Z ()) -> Db [t Identity]
delete' qf =
  let tName   = tableName (undefined :: t v)
      tFields = map ((tName ++ ".")++) $ names (undefined :: t v)
      (_, bq) = runQuery . qf . fmap1 (Exp . return . return . ((tName ++ ".") ++) . unFieldName) $ fieldNames
  in map buildRecord <$> B.baseDelete bq tName tFields

query :: forall r i a. (DBRecord r, AggrVal (IAggr i), AbstractType a r) => Query i Z (r (Expr i Z)) -> Db [a]
query = fmap (map fromAbstract) . query'

insert :: forall r i a. (DBTable r, AggrVal (IAggr i), AbstractType a r) => Query i Z (r (InsertExpr i)) -> Db [a]
insert = fmap (map fromAbstract) . insert'

update :: forall t a. (DBTable t, AbstractType a t) => (t (SingleExpr Z) -> Query Single Z (t UpdateExpr)) -> Db [a]
update = fmap (map fromAbstract) . update'

delete :: forall t a. (DBTable t, AbstractType a t) => (t (SingleExpr Z) -> Query Single Z ()) -> Db [a]
delete = fmap (map fromAbstract) . delete'