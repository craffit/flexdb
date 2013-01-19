{-# LANGUAGE TemplateHaskell, GADTs, TypeOperators, Rank2Types #-}
module DB.Flex.Create where

import Data.Char
import Data.Maybe
import Database.HDBC

import Data.Label.Util

import DB.Flex.Config
import DB.Flex.Monad
import DB.Flex.Record

import Language.Haskell.TH
import Language.Haskell.TH.Util

data FieldOpt a where
  Nullable :: FieldOpt (Maybe a)
--  Default  :: a -> FieldOpt a
  Primary  :: FieldOpt a
  Unique   :: FieldOpt a
  Foreign  :: DBTable t => t :> a -> FieldOpt a

data FieldOpts a = FieldOpts { fieldOpts :: [FieldOpt a] }

-- | Generate table instance from a datatype

mkTable :: (String -> String) -> Name -> Q [Dec]
mkTable f n = 
  do rec  <- dbRecord n 
     inst <- mkTableFields f (head rec)
     return $ rec ++ inst

mkTableFields :: (String -> String) -> Dec -> Q [Dec]
mkTableFields mkId (DataD _ tnm pars [RecC cns fs] _) =
     let ps = reverse $ tail $ reverse $ map getTV pars
         fieldNs = map (\(n,_,_) -> mkId (show n)) fs 
         inst = InstanceD [] (ConT (mkName "Functor1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                   [FunD (mkName "tableName") [Clause [WildP] (NormalB $ LitE $ StringL $ mkId $ show tnm) []]
                   ,FunD (mkName "fieldNames")
                       [Clause [] (NormalB $ foldl AppE (ConE cns) $ map (AppE (ConE $ mkName "FieldName") . LitE . StringL) fieldNs) []]
                   ]
     in return $ [ inst ]
mkTableFields _ _ = error "Not a valid datatype in mkTableFields"

-- | Generate a table datatype from a remote Postgres database

retrieveTable :: String -> String -> (String -> String) -> (SqlTypeId -> String) -> Config -> Q [Dec]
retrieveTable tbl hnm mkI mkT conf =
  do cols <- runIO $ withConnection conf $ flip describeTable tbl
     let mkCol (col, cDesc) = addNullable (fromMaybe True $ colNullable cDesc)
                                      (mkName $ mkI col, NotStrict, ConT $ mkName $ mkT $ colType cDesc)
         addNullable True  (n, s, t) = (n, s, AppT (ConT $ mkName "Maybe") t)
         addNullable False v         = v
     let dat  = DataD [] (mkName hnm) [] [RecC (mkName hnm) (map mkCol cols)] [mkName "Show", mkName "Eq"]
     rec <- dbRecord' dat
     let anm = (\(DataD _ v _ _ _) -> v) $ head rec
         inst = InstanceD [] (ConT (mkName "DBTable") `AppT` ConT anm)
                   [FunD (mkName "tableName") [Clause [WildP] (NormalB $ LitE $ StringL tbl) []]
                   ,FunD (mkName "fieldNames")
                       [Clause [] (NormalB $ foldl AppE (ConE anm) $ map (AppE (ConE $ mkName "FieldName") . LitE . StringL) $ map fst cols) []]
                   ]
     return $ [ dat, inst ] ++ rec

-- | Mapping SQL types to Haskell types
baseTypeInfo :: SqlTypeId -> String
baseTypeInfo SqlCharT              = "String"     
baseTypeInfo SqlVarCharT           = "String"     
baseTypeInfo SqlLongVarCharT       = "String"     
baseTypeInfo SqlWCharT             = "String"     
baseTypeInfo SqlWVarCharT          = "String"     
baseTypeInfo SqlWLongVarCharT      = "String"     
baseTypeInfo SqlDecimalT           = "Integer"    
baseTypeInfo SqlNumericT           = "Integer" 
baseTypeInfo SqlSmallIntT          = "Int"     
baseTypeInfo SqlIntegerT           = "Int"     
baseTypeInfo SqlRealT              = "Double"
baseTypeInfo SqlFloatT             = "Double"  
baseTypeInfo SqlDoubleT            = "Double"  
baseTypeInfo SqlBitT               = "Bool"    
baseTypeInfo SqlTinyIntT           = "Int"     
baseTypeInfo SqlBigIntT            = "Integer"    
baseTypeInfo SqlBinaryT            = "ByteString" 
baseTypeInfo SqlVarBinaryT         = "ByteString" 
baseTypeInfo SqlLongVarBinaryT     = "ByteString" 
baseTypeInfo SqlDateT              = "UTCTime"    
baseTypeInfo SqlTimeT              = "UTCTime"    
baseTypeInfo SqlTimeWithZoneT      = "UTCTime"    
baseTypeInfo SqlTimestampT         = "UTCTime"    
baseTypeInfo SqlTimestampWithZoneT = "UTCTime"    
baseTypeInfo SqlUTCDateTimeT       = "UTCTime"    
baseTypeInfo SqlUTCTimeT           = "UTCTime"    
baseTypeInfo (SqlIntervalT _)      = "SqlValue" 
baseTypeInfo SqlGUIDT              = "UUID"     
baseTypeInfo (SqlUnknownT "2950")  = "UUID"
baseTypeInfo (SqlUnknownT _)       = "SqlValue" 

withCase :: Eq a => a -> r -> (a -> r) -> a -> r
withCase a r f a' | a' == a = r
                  | otherwise = f a'

baseIdent :: String -> String
baseIdent = ("_" ++) . firstDown . mkData

firstDown :: String -> String
firstDown [] = []
firstDown (x:xs) = toLower x: xs

mkData :: String -> String
mkData ('_' : x : xs) = toUpper x : mkData xs
mkData (x : xs) | isAlphaNum x = x : mkData xs
                | otherwise    = mkData xs
mkData a = a

mkField :: String -> String
mkField (x:xs) | isAlphaNum x || x == '_' = toLower x : mkField xs
               | otherwise                = mkField xs
mkField a = a
