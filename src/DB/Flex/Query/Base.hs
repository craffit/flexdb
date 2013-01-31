{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module DB.Flex.Query.Base where

import Control.Applicative
import Control.Arrow
import Data.List hiding (group)
import Control.Monad
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as ST
import Database.HDBC
import Data.Label hiding (for)
import Data.Function

import DB.Flex.Monad

type BaseExpr a = State [SqlValue] a

runBase :: BaseExpr a -> a
runBase = fst . flip runState []

value :: SqlValue -> BaseExpr String
value v =
  do vs <- ST.get
     i <- case elemIndex v vs of
           -- Just i  | v /= SqlNull -> return (i + 1)
           _                      -> put (vs ++ [v]) >> return (length vs + 1)
     return $ "$" ++ show i

infixl 5 <>, <->

(<>) :: BaseExpr String -> BaseExpr String -> BaseExpr String
(<>) = liftM2 (++)

(<->) :: BaseExpr String -> BaseExpr String -> BaseExpr String
a <-> b = a <> return " " <> b

parens :: BaseExpr String -> BaseExpr String
parens = fmap $ \v -> "(" ++ v ++ ")"

list :: BaseExpr [String] -> BaseExpr String
list = parens . fmap (intercalate ", ")

unOp :: String -> BaseExpr String -> BaseExpr String
unOp v e = return v <-> parens e

postOp :: String -> BaseExpr String -> BaseExpr String
postOp v e = parens e <-> return v

binOp :: String -> BaseExpr String -> BaseExpr String -> BaseExpr String
binOp v e1 e2 = parens e1 <-> return v <-> parens e2

func :: String -> BaseExpr [String] -> BaseExpr String
func f exs = return f <-> list exs

data BaseQuery =
  BaseQuery
    { _tables   :: [BaseExpr String]
    , _restrict :: BaseExpr String
    , _having   :: BaseExpr String
    , _limit    :: Maybe Int
    , _offset   :: Maybe Int
    , _unique   :: Bool
    , _group    :: [BaseExpr String]
    , _order    :: [BaseExpr String]
    , _for      :: Maybe String
    }

$( mkLabel ''BaseQuery )

renderQuery :: BaseQuery -> BaseExpr [String] -> BaseExpr String
renderQuery bq project =
  fmap (intercalate " ") $ sequence
       [ return "select"
       , return $ if get unique bq then "distinct" else ""
       , intercalate ", " <$> project
       , if null $ get tables bq
           then return ""
           else ("from " ++) . intercalate ", " <$> sequence (get tables bq)
       , return "where"
       , get restrict bq
       , if null (get group bq)
           then return ""
           else (("group by "++) . intercalate ", ") <$> sequence (get group bq)
       , do h <- get having bq
            case h of
              "true" -> return ""
              _ -> return $ "having " ++ h
       , return $ maybe "" (\v -> "limit " ++ show v) $ get limit bq
       , return $ maybe "" (\v -> "offset " ++ show v) $ get offset bq
       , if null $ get order bq
           then return ""
           else ("order by " ++) . intercalate ", " <$> sequence (get order bq)
       , return $ maybe "" ("for " ++) $ get for bq
       ]

renderInsert :: BaseQuery -> String -> [(String, BaseExpr String)] -> BaseExpr String
renderInsert bq tab flds =
  let noDefs = filter ((/="DEFAULT") . runBase . snd) flds
  in fmap (intercalate " ") $ sequence
            [ return $ "insert into " ++  tab
            , return $ "(" ++ intercalate "," (map fst noDefs) ++ ")"
            , renderQuery bq (sequence $ map snd noDefs)
            , return "returning"
            , return $ intercalate "," (map fst flds)
            ]

renderUpdate :: BaseQuery -> String -> [(String, BaseExpr String)] -> BaseExpr String
renderUpdate bq tab flds =
  let noIgns = filter ((/="IGNORE") . runBase . snd) flds
  in  fmap (intercalate " ") $ sequence
            [ return $ "update " ++ tab ++ " set"
            , fmap (intercalate ",") $ mapM (\(f,v) -> return (f ++ " = ") <-> v) noIgns
            , if null (get tables bq)
                  then return ""
                  else ("from " ++) . intercalate ", " <$> sequence (get tables bq)
            , return "where"
            , get restrict bq
            , return "returning"
            , return $ intercalate "," (map (((tab ++ ".") ++) . fst) flds)
            ]

renderDelete :: BaseQuery -> String -> [String] -> BaseExpr String
renderDelete bq tab res =
  fmap (intercalate " ") $ sequence
            [ return $ "delete from " ++ tab
            , if null (get tables bq)
                then return ""
                else ("using " ++) . intercalate ", " <$> sequence (get tables bq)
            , return "where"
            , get restrict bq
            , return "returning"
            , return $ intercalate "," res
            ]

runBaseExpr :: BaseExpr String -> Db [[SqlValue]]
-- runBaseExpr = uncurry querySql . flip runState []
runBaseExpr = (\v -> unsafeIOToDb (print v) >> uncurry querySql v) . flip runState []

batchBaseExpr :: [BaseExpr String] -> Db [[[SqlValue]]]
batchBaseExpr = fmap concat
              . mapM ( (\v -> unsafeIOToDb (print $ fst v) >> uncurry executeBatch v)
                     . ((head . fst) &&& snd) . unzip)
              . groupBy ((==) `on` fst)
              . map (flip runState [])

baseQuery :: BaseQuery -> BaseExpr [String] -> Db [[SqlValue]]
baseQuery bq proj = runBaseExpr (renderQuery bq proj)

baseInsert :: BaseQuery -> String -> [(String, BaseExpr String)] -> Db [[SqlValue]]
baseInsert bq tab flds = runBaseExpr (renderInsert bq tab flds)

baseInsertMany :: String -> [(BaseQuery, [(String, BaseExpr String)])] -> Db [[[SqlValue]]]
baseInsertMany tab = batchBaseExpr . map (\(bq, flds) -> renderInsert bq tab flds)

baseUpdate :: BaseQuery -> String -> [(String, BaseExpr String)] -> Db [[SqlValue]]
baseUpdate bq tab flds = runBaseExpr (renderUpdate bq tab flds)

baseDelete :: BaseQuery -> String -> [String] -> Db [[SqlValue]]
baseDelete bq tab res = runBaseExpr (renderDelete bq tab res)
