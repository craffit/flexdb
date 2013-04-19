{-# LANGUAGE GeneralizedNewtypeDeriving
           , FlexibleInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , Rank2Types
           #-}
module DB.Flex.Monad where


import Control.Applicative
import Control.Exception
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS
import Control.Monad.Reader
import Database.HDBC hiding (execute)
import qualified Database.HDBC as HDBC

newtype DbT m a = DbT { unDbT :: ReaderT ConnWrapper m a }
  deriving (Functor, Applicative, Monad, MonadReader ConnWrapper, MonadFix)

type Db = DbT IO

runDb_ :: HDBC.IConnection c => c -> DbT m a -> m a
runDb_ conn = flip runReaderT (ConnWrapper conn) . unDbT

runDbIO_ :: HDBC.IConnection c => c -> Db a -> IO a
runDbIO_ conn db =
  do res <- runDb_ conn db `onException` rollback conn
     commit conn
     return res

maxDbTries :: Int
maxDbTries = 3

catchRecoverableExceptions :: IO a -> (SomeException -> IO a) -> IO a
catchRecoverableExceptions action handler = action `catches`
  [ Handler $ \(e :: AsyncException)            -> throwIO e
  , Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> throwIO e
  , Handler $ \(e :: BlockedIndefinitelyOnMVar) -> throwIO e
  , Handler $ \(e :: Deadlock)                  -> throwIO e
  , Handler $ \(e :: SomeException)             -> handler e
  ]

unsafeIOToDb :: IO a -> Db a
unsafeIOToDb = DbT . liftIO

runSql :: String -> [SqlValue] -> Db Integer
runSql q ps =
  do conn <- ask
     unsafeIOToDb $ run conn q ps

runSql_ :: String -> [SqlValue] -> Db ()
runSql_ q ps = () <$ runSql q ps

querySql :: String -> [SqlValue] -> Db [[SqlValue]]
querySql q ps =
  do conn <- ask
     unsafeIOToDb $ quickQuery' conn q ps

prepareSql :: String -> Db Statement
prepareSql q =
  do conn <- ask
     unsafeIOToDb $ prepare conn q

execute :: Statement -> [SqlValue] -> Db Integer
execute q = unsafeIOToDb . HDBC.execute q

executeMany :: Statement -> [[SqlValue]] -> Db ()
executeMany q = unsafeIOToDb . HDBC.executeMany q

fetchRow :: Statement -> Db (Maybe [SqlValue])
fetchRow = unsafeIOToDb . HDBC.fetchRow

executeBatch :: String -> [[SqlValue]] -> Db [[[SqlValue]]]
executeBatch q vs =
  do st <- prepareSql q
     forM vs $ \v ->
       do _ <- execute st v
          unsafeIOToDb $ fetchAllRows st