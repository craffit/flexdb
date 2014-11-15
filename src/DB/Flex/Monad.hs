{-# LANGUAGE GeneralizedNewtypeDeriving
           , FlexibleInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , Rank2Types
           , StandaloneDeriving
           #-}
module DB.Flex.Monad where


import Control.Applicative
import Control.Exception
import qualified Control.Monad.CatchIO as C
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS
import Control.Monad.Reader
import Database.HDBC hiding (execute)
import qualified Database.HDBC as HDBC

newtype DbT m a = DbT { unDbT :: ReaderT ConnWrapper m a }
  deriving (Functor, Applicative, Monad, MonadReader ConnWrapper, MonadFix, C.MonadCatchIO, MonadPlus, MonadIO, Alternative)

class (Functor m, MonadIO m) => DbMonad m where
  askConn :: m ConnWrapper

instance (Functor m, MonadIO m) => DbMonad (DbT m) where askConn = ask
instance (Error e, DbMonad m) => DbMonad (ErrorT e m) where askConn = lift askConn
instance DbMonad m => DbMonad (ReaderT d m) where askConn = lift askConn

runDb_ :: (MonadIO m, HDBC.IConnection c) => c -> DbT m a -> m a
runDb_ conn (DbT db) =
  do res <- runReaderT db (ConnWrapper conn)
     liftIO $ commit conn
     return res

runDbIO_ :: HDBC.IConnection c => c -> DbT IO a -> IO a
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

unsafeIOToDb :: DbMonad m => IO a -> m a
unsafeIOToDb = liftIO

runSql :: DbMonad m => String -> [SqlValue] -> m Integer
runSql q ps =
  do conn <- askConn
     unsafeIOToDb $ run conn q ps

runSql_ :: DbMonad m => String -> [SqlValue] -> m ()
runSql_ q ps = () <$ runSql q ps

querySql :: DbMonad m => String -> [SqlValue] -> m [[SqlValue]]
querySql q ps =
  do conn <- askConn
     unsafeIOToDb $ quickQuery' conn q ps

prepareSql :: DbMonad m => String -> m Statement
prepareSql q =
  do conn <- askConn
     unsafeIOToDb $ prepare conn q

execute :: DbMonad m =>  Statement -> [SqlValue] -> m Integer
execute q = unsafeIOToDb . HDBC.execute q

executeMany :: DbMonad m => Statement -> [[SqlValue]] -> m ()
executeMany q = unsafeIOToDb . HDBC.executeMany q

fetchRow :: DbMonad m => Statement -> m (Maybe [SqlValue])
fetchRow = unsafeIOToDb . HDBC.fetchRow

executeBatch :: DbMonad m => String -> [[SqlValue]] -> m [[[SqlValue]]]
executeBatch q vs =
  do st <- prepareSql q
     forM vs $ \v ->
       do _ <- execute st v
          unsafeIOToDb $ fetchAllRows st
