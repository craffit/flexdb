{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Config where

import Prelude hiding ((.), id)

import Control.Category
import Data.Char
import Data.Label
import Data.List
import Data.List.Split
import Data.Maybe
import Paths_flexdb
import System.Console.GetOpt

import qualified Config.Parse as C
import qualified DB.Flex.Config    as Db

data Config = Config
  { _dbConfig   :: Db.Config
  , _target     :: String
  , _baseModule :: String
  , _includes   :: [String]
  , _configFile :: String
  , _replaces   :: [(String, String)]
  } deriving Show

mkLabels [''Config]

defaultConfig :: Config
defaultConfig = Config
  { _dbConfig   = Db.defaultConfig
  , _target     = "./"
  , _baseModule = ""
  , _includes   = []
  , _configFile = "dbconfig"
  , _replaces   = []
  }

splitArg :: String -> String -> [String]
splitArg ch = filter (not . null) . map (reverse . dropWhile isSpace . reverse . dropWhile isSpace) . splitOn ch

mkReplace :: String -> [(String, String)]
mkReplace = catMaybes . map (mk . splitArg "|"). splitArg ","
  where mk [x,y] = Just (x,y)
        mk _     = Nothing

options :: [OptDescr (Config -> Config)]
options =
  Db.options dbConfig ++
  [ Option ['t'] ["target"]     (ReqArg (set target) "DIRECTORY") "The directory to which generated code is written."
  , Option ['m'] ["module"]     (ReqArg (set baseModule) "MODULE") "The base module for the generated code."
  , Option ['i'] ["include"]    (ReqArg (\v -> modify includes (splitArg "," v++)) "MODULE(,MODULE)*") "The base module for the generated code."
  , Option ['c'] ["configfile"] (ReqArg (set configFile) "FILE") "A file to read configuration options from. The options are the same as the command line options. The default is dbconfig."
  , Option ['r'] ["replace"]    (ReqArg (\v -> modify replaces (mkReplace v ++)) "NAME|NAME(,NAME|NAME)*") "Replace a field in the generated code for another field."
  ]

getConfig :: IO Config
getConfig = C.getConfig version $(C.gitVersion) defaultConfig options (get configFile)

printFlags :: IO ()
printFlags = putStrLn . intercalate "," . concatMap longFlag $ options

longFlag :: OptDescr a -> [String]
longFlag (Option _ lfl _ _) = lfl
