{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module DB.Flex.Generate where

import Data.Label
import Data.List
import Data.Maybe
import Database.HDBC
import DB.Flex.Create
import Language.Haskell.TH
import System.FilePath

data Config = Config
  { _target     :: String
  , _baseModule :: String
  , _includes   :: [String]
  , _configFile :: String
  , _replaces   :: [(String, String)]
  } deriving Show

$( mkLabels [''Config] )

replaceFunc :: [(String, String)] -> String -> String
replaceFunc vs v = fromMaybe v $ lookup v vs

generateBindings :: Config -> ConnWrapper -> IO ()
generateBindings config conn =
    do tbs <- getTables conn
       mapM_ (mkModule config conn) tbs

mkModule :: Config -> ConnWrapper -> String -> IO ()
mkModule config conn tbl =
  do let repl  = replaceFunc (get replaces config)
         dName = firstUp $ repl tbl
     decs <- runQ $ retrieveTable tbl dName (baseIdent . repl) baseTypeInfo (return conn)
     writeFile (get target config </> (dName ++ ".hs")) $
       intercalate "\n" $
         [ "{-# LANGUAGE RankNTypes, KindSignatures, FlexibleContexts, MultiParamTypeClasses, NoMonomorphismRestriction, TypeFamilies, UndecidableInstances, OverlappingInstances #-}"
         , "{- This code is generated, changes to this code might be overwritten! -}"
         , "module " ++ get baseModule config ++ "." ++ dName ++ " where"
         , "import Control.Arrow"
         , "import Data.Label.Abstract"
         , "import DB.Flex"
         ]
         ++ map ("import "++) (get includes config)
         ++ map pprint decs



