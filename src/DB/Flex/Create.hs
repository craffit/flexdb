{-# LANGUAGE TemplateHaskell, GADTs, TypeOperators, Rank2Types, ScopedTypeVariables, KindSignatures #-}
module DB.Flex.Create where

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe
import Data.Proxy
import Database.HDBC

import Data.Foldable1
import Data.Zippable1
import Data.Label.Util

import DB.Flex.Config
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Base
import DB.Flex.Query


import Language.Haskell.TH
import Language.Haskell.TH.Util

import Safe

data FieldOpt a  where
  Nullable :: FieldOpt (Maybe a)
  NotNull  :: FieldOpt a
  Def      :: Eq a => a -> FieldOpt a
  Primary  :: FieldOpt a
  Unique   :: FieldOpt a
  Foreign  :: DBTable t => t :> a -> FieldOpt a
  Type     :: String -> FieldOpt a

instance Eq (FieldOpt a) where
  Nullable  == Nullable = True
  NotNull   == NotNull  = True
  (Def a)   == (Def b)  = a == b
  Primary   == Primary  = True
  Unique    == Unique   = True
  (Foreign (l :: t :> x)) == (Foreign (l' :: t' :> x'))  =
    let nms  = fieldNames :: t FieldName
        nms' = fieldNames :: t' FieldName
    in tableName nms == tableName nms' && unFieldName (nms |.| l) == unFieldName (nms' |.| l')
  (Type a)  == (Type b) = a == b
  _ == _ = False

data FieldOpts a = FieldOpts { fieldOpts :: [FieldOpt a] }

renderCreate :: forall a. DBTable a => a FieldOpts -> BaseExpr String
renderCreate fs =
  let mkForeign (Foreign (l :: (t :> x))) =
         let nms = fieldNames :: t FieldName
         in Just $ "references " ++ tableName nms ++ " (" ++ unFieldName (nms |.| l) ++ ")"
      mkForeign _ = Nothing
      renderField nm ops =
        fmap (intercalate " ") $ sequence
                    [ return nm
                    , return $ head $ [ v | Type v <- ops ] ++ [ "text" ]
                    , return $ if any (==NotNull) ops then "not null" else "null"
                    , maybe (return "") (liftM ("default " ++) . value . toSql) $ headMay $ [ v | Def v <- ops ]
                    , return $ if any (==Primary) ops then "primary key" else ""
                    , return $ if any (==Unique) ops then "unique" else ""
                    , return $ intercalate " " $ catMaybes $ map mkForeign ops
                    ]
  in fmap (intercalate " ") $ sequence
            [ return $ "create table if not exists " ++ tableName fs ++ " ("
            , fmap (intercalate ", ") $ sequence $ collect unConstVal 
                  $ zip1 (\(FieldName nm) (Tup1 Field (FieldOpts ops)) -> ConstVal $ renderField nm ops) fieldNames 
                  $ zip1 Tup1 recordFields fs
            , return ")"
            ]

createTable :: forall a . DBTable a => a FieldOpts -> Db ()
createTable = fmap (const ()) . runBaseExpr . renderCreate

dropTable :: forall a f. DBTable a => Proxy (a f) -> Db ()
dropTable _ = fmap (const ()) $ querySql ("drop table if exists " ++ tableName (fieldNames :: a FieldName)) []

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
         inst = InstanceD [] (ConT (mkName "DBTable") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
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

withCase :: Eq r => r -> r -> (a -> r) -> a -> r
withCase a r f a' | f a' == a = r
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
