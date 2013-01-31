module Main where

import Config
import Data.Label
import Data.List
import Data.Maybe
import Database.HDBC
import DB.Flex.Create
import DB.Flex.Monad
import Language.Haskell.TH
import System.FilePath

main :: IO ()
main =
  do config <- getConfig
     putStrLn "Retrieving tables"
     tbs <- withConnection (get dbConfig config) getTables
     mapM_ (mkModule config) tbs

replaceFunc :: [(String, String)] -> String -> String
replaceFunc vs v = fromMaybe v $ lookup v vs

mkModule :: Config -> String -> IO ()
mkModule config tbl =
  do let repl  = replaceFunc (get replaces config)
         dName = firstUp $ repl tbl
     decs <- runQ $ retrieveTable tbl (dName) (baseIdent . repl) baseTypeInfo $ get dbConfig config
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



