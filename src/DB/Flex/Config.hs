{-# LANGUAGE TemplateHaskell, TupleSections, TypeOperators #-}
module DB.Flex.Config where

import Prelude hiding ((.))

import Control.Category
import Data.Label
import Data.List
import Data.Maybe
import Safe
import System.Console.GetOpt

data Config = Config
  { _name :: String
  , _user :: String
  , _pass :: String
  , _host :: Maybe String
  , _port :: Maybe Int
  } deriving Show

mkLabels [''Config]

defaultConfig :: Config
defaultConfig = Config "postgres" "postgres" (error "No database password specified.") (Just "127.0.0.1") (Just 5432)

options :: (a :-> Config) -> [OptDescr (a -> a)]
options parent =
  [ Option [] ["dbname"]     (ReqArg (set (name . parent)) "DBNAME") "The name of the database containing Silk data. Default 'silk'."
  , Option [] ["dbuser"]     (ReqArg (set (user . parent)) "USERNAME") "The name of the database user. Default 'silk'"
  , Option [] ["dbpassword"] (ReqArg (set (pass . parent)) "PASSWORD") "The password for the database user. This option is always required, either on the command line or in the config file."
  , Option [] ["dbhost"]     (ReqArg (set (host . parent) . Just) "HOST") "The host name or IP for the database server. Default '127.0.0.1'."
  , Option [] ["dbport"]     (ReqArg (set (port . parent) . Just . readNote "Database port must be numeric.") "PORT") "The port number to use when connecting to the database. Default '5432'."
  ]

configToString :: Config -> String
configToString config = intercalate " " . map pairToString $
  [ ("dbname",   get name config)
  , ("user",     get user config)
  , ("password", get pass config)
  ]
  ++
  map ("host",)          (maybeToList . get host $ config)
  ++
  map (("port",) . show) (maybeToList . get port $ config)
  where
    pairToString (n, v) = n ++ "=" ++ v
