{-# LANGUAGE TemplateHaskell, GADTs, TypeOperators, Rank2Types, ScopedTypeVariables, KindSignatures #-}
module DB.Flex.Create where

import Data.Char
import Data.List
import Data.Maybe
import Data.Proxy
import Database.HDBC

import Data.Functor1
import Data.Foldable1
import Data.Zippable1
import Data.Label.Util

import DB.Flex.Config
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Table
import DB.Flex.Query.Base (BaseExpr, runBaseExpr)
import DB.Flex.Query.Core
import DB.Flex.Query

import Language.Haskell.TH hiding (Exp)
import Language.Haskell.TH.Util

import Safe

renderCreate :: forall a f. TableDef a => Proxy (a f)  -> BaseExpr String
renderCreate _ =
  let tbOpts = tableOpts :: [TableOpt a]
      fOpts  = fieldOpts :: a FieldOpts
      mkForeign (Foreign (l :: (t :> x)) ac) =
         let nms = fieldNames :: t FieldName
         in Just $ "references " ++ tableName nms ++ " (" ++ unFieldName (nms |.| l) ++ ") on delete " ++ show ac
      mkForeign _ = Nothing

      renderFieldOpt :: forall x. DBType x => String -> [FieldOpt x] -> BaseExpr String
      renderFieldOpt nm ops =
        let defNullable = nullable (Proxy :: Proxy x)
            defType     = typeRep (Proxy :: Proxy x)
        in fmap (intercalate " ") $ sequence
                    [ return nm
                    , return $ headDef defType $ [ v | Type v <- ops ]
                    , return $ if any (==Nullable) ops || (all (/=NotNull) ops && defNullable) 
                                then "null" 
                                else "not null"
                    , fromMaybe (return "") $ headMay $ 
                          [ fmap (\e -> "default (" ++ e ++ ")") $ runExp v 
                          | Def v <- ops ]
                    , return $ if any (==Primary) ops then "primary key" else ""
                    , return $ if any (==Unique) ops then "unique" else ""
                    , return $ intercalate " " $ catMaybes $ map mkForeign ops
                    , fmap (intercalate " ") $ sequence $ 
                          [ fmap (\e -> "check (" ++ e ++ ")") $ runExp (f (pureExp nm)) 
                          | Check f <- ops ]
                    ]

      renderTableOpt (TableUnique fs)  = return $ "unique (" ++ intercalate ", " (map (\(Label l) -> unFieldName (fieldNames |.| l)) fs) ++ ")"
      renderTableOpt (TablePrimary fs) = return $ "primary key (" ++ intercalate ", " (map (\(Label l) -> unFieldName (fieldNames |.| l)) fs) ++ ")"
      renderTableOpt (TableCheck chk)  = fmap (\e -> "check (" ++ e ++ ")") . runExp . chk . fmap1 (Exp . return . return . unFieldName) $ fieldNames

  in fmap (intercalate " ") $ sequence
            [ return $ "create table if not exists " ++ tableName fOpts ++ " ("
            , fmap (intercalate ", ") $ sequence $ 
                (collect unConstVal 
                  $ zip1 (\(FieldName nm) (Tup1 Field (FieldOpts ops)) -> ConstVal $ renderFieldOpt nm ops) fieldNames 
                  $ zip1 Tup1 recordFields fOpts)
                ++
                map renderTableOpt tbOpts
            , return ")"
            ]

createTable :: forall a f. TableDef a => Proxy (a f) -> Db ()
createTable = fmap (const ()) . runBaseExpr . renderCreate

dropTable :: forall a f. Table a => Proxy (a f) -> Db ()
dropTable _ = fmap (const ()) $ querySql ("drop table if exists " ++ tableName (fieldNames :: a FieldName)) []

-- | Generate table instance from a datatype

mkTable :: (String -> String) -> Name -> Q [Dec]
mkTable f n = 
  do TyConI d <- reify n
     rec  <- dbRecord' d 
     inst <- mkTableFields f (head rec)
     viewInst <- mkAbstractView' d (head rec)
     return $ rec ++ inst ++ viewInst

mkTableFields :: (String -> String) -> Dec -> Q [Dec]
mkTableFields mkId (DataD _ tnm pars [RecC cns fs] _) =
     let ps = reverse $ tail $ reverse $ map getTV pars
         fieldNs = map (\(n,_,_) -> mkId (show n)) fs 
         inst = InstanceD [] (ConT (mkName "Table") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
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
     viewInst <- mkAbstractView' dat (head rec)
     let anm = (\(DataD _ v _ _ _) -> v) $ head rec
         inst = InstanceD [] (ConT (mkName "Table") `AppT` ConT anm)
                   [FunD (mkName "tableName") [Clause [WildP] (NormalB $ LitE $ StringL tbl) []]
                   ,FunD (mkName "fieldNames")
                       [Clause [] (NormalB $ foldl AppE (ConE anm) $ map (AppE (ConE $ mkName "FieldName") . LitE . StringL) $ map fst cols) []]
                   ]
     return $ [ dat, inst ] ++ rec ++ viewInst

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
