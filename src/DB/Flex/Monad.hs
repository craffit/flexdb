{-# LANGUAGE GeneralizedNewtypeDeriving
           , FlexibleInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           #-}
module DB.Flex.Monad
  ( Db
  , runDb
  , runDbIO
  , runDbWith
  , unsafeIOToDb
  , withConnection

  , runSql
  , runSql_
  , querySql
  , prepareSql
  , execute
  , executeMany
  , fetchRow

  , MonadDb (..)

  , DbMonad
  , runDbMonad

  , SqlValue
  , Statement
  , toSql
  , fromSql
  ) where

import Prelude hiding (catch)

import Control.Applicative
import Control.Exception
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.List
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Database.HDBC (SqlValue, Statement, commit, fromSql, prepare, quickQuery', rollback, run, toSql)
import Database.HDBC.PostgreSQL
import System.Log.Logger
import qualified Database.HDBC as HDBC

import DB.Flex.Config

newtype Db a = Db { unDb :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadReader Connection, MonadFix)

class (Applicative m, MonadIO m) => MonadDb m where
  askDbConfig :: m Config

instance MonadDb m => MonadDb (ContT r m) where
  askDbConfig = lift askDbConfig

instance (Error e, MonadDb m) => MonadDb (ErrorT e m) where
  askDbConfig = lift askDbConfig

instance MonadDb m => MonadDb (ListT m) where
  askDbConfig = lift askDbConfig

-- instance MonadDb m => MonadDb (MaybeT m) where
--   askDbConfig = lift askDbConfig

instance (Monoid w, MonadDb m) => MonadDb (RWST r w s m) where
  askDbConfig = lift askDbConfig

instance MonadDb m => MonadDb (ReaderT r m) where
  askDbConfig = lift askDbConfig

instance MonadDb m => MonadDb (StateT s m) where
  askDbConfig = lift askDbConfig

instance (Monoid w, MonadDb m) => MonadDb (WriterT w m) where
  askDbConfig = lift askDbConfig

newtype DbMonad m a = DbMonad { unDbMonad :: ReaderT Config m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (Applicative m, MonadIO m) => MonadDb (DbMonad m) where
  askDbConfig = DbMonad ask

instance MonadBase b m => MonadBase b (DbMonad m) where
  liftBase = liftBaseDefault

instance MonadTransControl DbMonad where
  newtype StT DbMonad a = StTDbMonad { unStTDbMonad :: StT (ReaderT Config) a }
  liftWith f = DbMonad (liftWith (\runt -> f (liftM StTDbMonad . runt . unDbMonad)))
  restoreT = DbMonad . restoreT . liftM unStTDbMonad

instance MonadBaseControl b m => MonadBaseControl b (DbMonad m) where
  newtype StM (DbMonad m) a = StMDbMonad { unStMDbMonad :: ComposeSt DbMonad m a }
  liftBaseWith = defaultLiftBaseWith StMDbMonad
  restoreM = defaultRestoreM unStMDbMonad

runDbMonad :: Config -> DbMonad m a -> m a
runDbMonad config = flip runReaderT config . unDbMonad

runDb_ :: Connection -> Db a -> IO a
runDb_ conn = flip runReaderT conn . unDb

maxDbTries :: Int
maxDbTries = 3

runDbIO :: Config -> Db a -> IO a
runDbIO config db = runDbIO' 1
  where
    runDbIO' n = runDbIO_ config db `catchRecoverableExceptions` handler n
    handler n (SomeException e) =
      if n < maxDbTries
        then
          do warningM "Db" ("Exception during database action, retrying: " ++ show e)
             runDbIO' (n + 1)
        else throwIO e

catchRecoverableExceptions :: IO a -> (SomeException -> IO a) -> IO a
catchRecoverableExceptions action handler = action `catches`
  [ Handler $ \(e :: AsyncException)            -> throwIO e
  , Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> throwIO e
  , Handler $ \(e :: BlockedIndefinitelyOnMVar) -> throwIO e
  , Handler $ \(e :: Deadlock)                  -> throwIO e
  , Handler $ \(e :: SomeException)             -> handler e
  ]

runDbIO_ :: Config -> Db a -> IO a
runDbIO_ config db = withConnection config $ \conn -> do
  res <- runDb_ conn db `onException` rollback conn
  commit conn
  return res

runDbWith :: MonadIO m => Config -> Db a -> m a
runDbWith config = liftIO . runDbIO config

runDb :: MonadDb m => Db a -> m a
runDb act = askDbConfig >>= flip runDbWith act

withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection = withPostgreSQL . configToString

unsafeIOToDb :: IO a -> Db a
unsafeIOToDb = Db . liftIO

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
