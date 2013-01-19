{-# LANGUAGE TupleSections, TemplateHaskell #-}
module DB.Flex.Config where

import Data.List
import Data.Maybe

data Config = Config
  { dbname   :: String
  , user     :: String
  , password :: String
  , host     :: Maybe String
  , port     :: Maybe Int
  } deriving Show

configToString :: Config -> String
configToString config = intercalate " " . map pairToString $
  [ ("dbname",   dbname   config)
  , ("user",     user     config)
  , ("password", password config)
  ]
  ++
  map ("host",)          (maybeToList . host $ config)
  ++
  map (("port",) . show) (maybeToList . port $ config)
  where
    pairToString (n, v) = n ++ "=" ++ v
