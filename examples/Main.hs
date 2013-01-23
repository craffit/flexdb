{-# LANGUAGE TemplateHaskell, KindSignatures, FlexibleContexts, MultiParamTypeClasses
           , PackageImports, NoMonomorphismRestriction, TypeFamilies, UndecidableInstances
           , OverlappingInstances #-}
module Main where

import Control.Monad
import DB.Flex
import Data.Convertible
import Data.UUID
import Data.Label
import Data.Time.Clock
import Data.Proxy

import Language.Haskell.TH hiding (Foreign)

import Safe
import System.Random

conf = Config "database" "user" "password" (Just "localhost") (Just 5432)
runTest = runDbWith conf

-- | Example data definitions

data User = 
  User
    { _uuid       :: UUID
    , _alias      :: String
    , _email      :: String
    , _password   :: String
    , _registered :: UTCTime
    } deriving (Eq, Show)

$( mkTable (withCase "user" "users" mkField) ''User )
$( mkLabels [''User, ''User'] )

userTable :: User' FieldOpts
userTable =
  User'
    { _uuid'       = FieldOpts [Primary]
    , _alias'      = FieldOpts []
    , _email'      = FieldOpts [Unique]
    , _password'   = FieldOpts []
    , _registered' = FieldOpts [Def now]
    }

data Document =
  Document
    { _did      :: Int
    , _name     :: String
    , _owner    :: UUID
    , _content  :: String
    , _created  :: UTCTime
    , _modified :: UTCTime
    } deriving (Eq, Show)

$( mkTable mkField ''Document )
$( mkLabels [''Document, ''Document' ])

documentTable :: Document' FieldOpts
documentTable =
  Document'
    { _did'      = FieldOpts [Type "serial",Primary]
    , _name'     = FieldOpts []
    , _owner'    = FieldOpts [Foreign uuid' Cascade]
    , _content'  = FieldOpts []
    , _created'  = FieldOpts [NotNull, Def now]
    , _modified' = FieldOpts [NotNull, Def now]
    }

data Role = Reader | Writer | Administrator deriving (Eq, Show, Read)

instance Convertible SqlValue Role where safeConvert = fmap read . safeConvert
instance Convertible Role SqlValue where safeConvert = safeConvert . show
instance DBType Role where typeRep _ = "text"

data Permission =
  Permission
    { _pid      :: Int
    , _role     :: Role
    , _user     :: UUID
    , _document :: Int
    } deriving (Eq, Show)

$( mkTable mkField ''Permission )
$( mkLabels [''Permission, ''Permission' ])

permissionTable :: Permission' FieldOpts
permissionTable =
  Permission'
    { _pid'      = FieldOpts [Type "serial", Primary]
    , _role'     = FieldOpts []
    , _user'     = FieldOpts [Foreign uuid' Cascade]
    , _document' = FieldOpts [Foreign did' Cascade]
    }

-- | Creating a database

createDB :: IO ()
createDB =
  do runTest $ do dropTable (Proxy :: Proxy (Permission' f))
                  dropTable (Proxy :: Proxy (Document' f))
                  dropTable (Proxy :: Proxy (User' f))
                  createTable userTable []
                  createTable documentTable [ TableUnique [Label name', Label owner']
                                            , TableCheck $ \tab -> tab |.| created' .<=. tab |.| modified']
                  createTable permissionTable [TableUnique [Label user', Label document']]

-- | Storing data

newUser :: String -> String -> String -> IO [User]
newUser nm ml pw =
  do time <- getCurrentTime
     uid <- randomIO
     runTest $ insert $ return $ User' (con uid) (con nm) (con ml) (con pw) (con time)


newDocument :: String -> String -> String -> IO [Document]
newDocument doc cont usr = runTest $ 
    insert $ do user <- tableSieve $ \tab -> tab |.| email' .==. con usr
                return $ name'     |->>| doc
                       $ content'  |->>| cont
                       $ owner'    |->|  user |.| uuid'
                       $ defaultInsert


newPermission :: String -> String -> String -> Role -> IO [Permission]
newPermission docOwner doc usr rol = runTest $ 
        insert $ do user    <- tableSieve $ \tab -> tab |.| email' .==. con usr
                    docUser <- tableSieve $ \tab -> tab |.| email' .==. con docOwner
                    docu    <- tableSieve $ \tab -> tab |.| name'  .==. con doc
                                               .&&. tab |.| owner' .==. docUser |.| uuid'
                    return $ role'     |->>| rol
                           $ document' |->|  docu |.| did'
                           $ user'     |->|  user |.| uuid'
                           $ defaultInsert

demoData :: IO ()
demoData =
  do newUser "bram" "bram@silkapp.com" "secret"
     newUser "erik" "erik@silkapp.com" "secret"
     newUser "sebas" "sebas@silkapp.com" "secret"
     newDocument "Foobaz" "Zecontent" "bram@silkapp.com"
     newDocument "Foobaz2" "Zecontentz" "bram@silkapp.com"
     newDocument "Foobaz" "Zecontentz" "erik@silkapp.com"
     newPermission "bram@silkapp.com" "Foobaz" "erik@silkapp.com" Administrator
     newPermission "bram@silkapp.com" "Foobaz" "sebas@silkapp.com" Administrator
     newPermission "erik@silkapp.com" "Foobaz" "bram@silkapp.com" Administrator
     return ()

-- | Encoding a type-level ontology of the database

instance Foreign Document' User' where
  type ForeignKey Document' User' = UUID
  foreignKey = owner' >-< uuid'

instance Foreign Permission' User' where
  type ForeignKey Permission' User' = UUID
  foreignKey = user' >-< uuid'

instance Foreign Permission' Document' where
  type ForeignKey Permission' Document' = Int
  foreignKey = document' >-< did'



instance RecordSelector User' where
  type RecordKey User' = String
  keyField = email'

instance PrimarySelector User'


instance RecordSelector Document' where
  type RecordKey Document' = String
  keyField = name'

instance ChildSelector Document' where
  type ParentTable Document' = User'


-- | The ontology can be used to write shorter queries

newDocument' :: String -> String -> String -> IO [Document]
newDocument' doc cont usr = 
  runTest $ insert $ withParent (selector usr) $ name' |->>| doc $ content' |->>| cont $ defaultInsert

newPermission' :: String -> String -> String -> Role -> IO [Permission]
newPermission' docOwner doc usr rol = runTest $ 
        insert $ do user    <- selector usr
                    docu    <- selector docOwner >>= child doc
                    return $ role'     |->>| rol
                           $ document' |->| docu |.| did'
                           $ user'     |->| user |.| uuid'
                           $ defaultInsert

ownsDocuments :: String -> Query i l (Document' (SingleExpr l))
ownsDocuments = selectorChildren <=< selector

permittedDocuments :: String -> Query i l (Document' (SingleExpr l))
permittedDocuments = did' >*< document' <=< user' >*< uuid' <=< selector

administratorDocuments :: String -> Query i l (Document' (SingleExpr l))
administratorDocuments = did' >*< document'
                     <=< sieve (\tab -> tab |.| role' .==. con Administrator) 
                     <=< user' >*< uuid'
                     <=< selector

userDocuments :: String -> Query i l (Document' (SingleExpr l))
userDocuments u = ownsDocuments u `union` permittedDocuments u

-- | Extract data from a database using a custom datatype

data DocCount = DocCount { userEmail :: String, documents :: Int } deriving Show

$( dbRecord ''DocCount )

-- | Flexdb supports aggregate queries

mostOwned :: IO [DocCount]
mostOwned =
  runTest $ query $
    do users <- table
       docs <- selectorChildren users
       fromU <- group $ users |.| email'
       let dCount = count (docs |.| did')
       having $ dCount .>. con 1
       order    $ desc $ dCount
       return $ DocCount' fromU dCount

userDocs :: String -> IO [Document]
userDocs = runTest . query . userDocuments

-- | Defining 'views' on the database, which aggregate data from multiple table into one haskell data structure
data UserInfo p d =
  UserInfo
    { _uName        :: String
    , _uEmail       :: String
    , _uPermissions :: p
    , _uDocuments   :: d
    } deriving (Eq, Show)

$( mkLabel ''UserInfo )

instance ( FieldJoin Permission'  UUID MultiChild Create p
         , FieldJoin Document'    UUID MultiChild Create d
         ) => View (UserInfo p d) where
  type ViewTable (UserInfo p d) = User'
  viewQuery = ViewQuery (const $ emptyData UserInfo) (const defaultInsert)
                [ uName        |= alias'
                , uEmail       |= email'
                , uPermissions |+ mkFieldJoin (uuid' >-< user')  MultiChild Create
                , uDocuments   |+ mkFieldJoin (uuid' >-< owner') MultiChild Create
                ]

users :: IO [UserInfo [Permission] [Document]]
users = runTest queryAll
