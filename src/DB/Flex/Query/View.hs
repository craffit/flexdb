{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators, KindSignatures
           , OverlappingInstances, ScopedTypeVariables, FlexibleInstances, FlexibleContexts
           , UndecidableInstances, TypeSynonymInstances, TupleSections, GADTs, TypeFamilies
           , Rank2Types, ImpredicativeTypes, DataKinds, PolyKinds #-}
module DB.Flex.Query.View where

import Control.Arrow
import Control.Monad.Identity hiding (forM_)

import Data.Convertible
import Data.Label
import Data.Label.Util

import Data.Zippable1

import Data.Foldable hiding (concat, foldl)
import Data.Maybe

import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Typed 
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Util

import Safe

-- | A class for datatypes which give rise to a database query

class Queryable a where
  type QueryTable a :: (* -> *) -> *
  filterQuery :: a -> QueryTable a (SingleExpr l) -> Query i l (QueryTable a (SingleExpr l))

fieldFilter :: (Table t, Eq x, Convertible x SqlValue) 
            => a :-> x -> t :> x -> a 
            -> t (SingleExpr l) -> Query i l (t (SingleExpr l))
fieldFilter vField dField v = sieve (\tab -> tab |.| dField .==. constant (v |.| vField))


-- | Main type class for describing a Haskell 'view' on a database table. This view can aggregate data from
-- others tables and specifies the way in which the data structure can be saved.

class (Table (ViewTable a), Eq a) => View a where
  -- | The table for which the view is defined
  type ViewTable a :: (* -> *) -> *
  viewQuery :: ViewQuery a (ViewTable a)

-- | A view query is just a list of 'Fields' which can be specified.
data ViewQuery a t = 
  ViewQuery 
    { makeView   :: t Identity -> a
    , storeView  :: a -> t (InsertExpr Single)
    , viewFields :: [ViewField a t]
    }

-- | A field consists of a lens which serves to put data into a strcuture and get it out again.
-- The FieldType specifies how this data is retrieved from the database. The savDefault specifies whether the field gets a default value when stored in the database.

data ViewField a t where
  ViewField :: { fieldValue  :: a :-> v
               , fieldType   :: FieldType t v
               , saveDefault :: Default
               } -> ViewField a t

-- | The Fieldtype constructor describes the ways in which data can be aggregated from the database.
--
-- The Base constructor means that a field in the view is taken directly from the table
--
-- The Join constructor describes that the value for this field comes from another table. 
-- It specifies two join fields and the relation that this join with the other table.

data FieldType t v where
  Empty :: FieldType t ()
  Base  :: Convertible x SqlValue => t :> x -> FieldType t x
  Join  :: (View o, Ord x, Convertible x SqlValue, Convertible SqlValue x) =>
         { fieldJoin :: (t :><: ViewTable o) x
         , relation  :: Relation o fv
         , creation  :: Creation (ViewTable o) o
         } -> FieldType t fv

-- | Description of join relations. Describes what the remote table is to the current table.
data Relation o v where
  ParentV      :: Relation o o
  SingleChildV :: Relation o o
  OptionChildV :: Relation o (Maybe o)
  MultiChildV  :: Relation o [o]

-- | Represents how joined tables should be handled during insertion and update
data Creation t o where
  CreateV ::                                    Creation t o
  RelateV :: (Queryable o, t ~ QueryTable o) => Creation t o
  IgnoreV ::                                    Creation t o

data Default = Default | NoDefault

-- | Convenient combinators for constructing views

infix 5 |+, |=, |?

(|+) :: a :-> v -> FieldType t v -> ViewField a t
to |+ aggr = ViewField to aggr NoDefault

(|=) :: Convertible v SqlValue => a :-> v -> t :> v -> ViewField a t
to |= from = ViewField to (Base from) NoDefault

(|?) :: Convertible v SqlValue => a :-> v -> t :> v -> ViewField a t
to |? from = ViewField to (Base from) Default

foldrM' :: Monad m => b -> [a] -> (a -> b -> m b) -> m b
foldrM' b l f = foldrM f b l

-- | Generic function for querying views. Will aggregate all appropriate data as specified in the viewQuery from View
performViewQuery :: forall a i. View a => Query i Z (ViewTable a (Expr i Z)) -> Db [(ViewTable a Identity, a)]
performViewQuery q =
   do dat <- query' q
      let ViewQuery mk _ fields = viewQuery :: ViewQuery a (ViewTable a)
          empt                  = fmap (id &&& mk) dat
      foldrM' empt fields $ \(ViewField field fType _) ac ->
          case fType of
            Empty  -> return $ map (second $ set field ()) ac
            Base f -> return $ map (\(v,a) -> (v, set field (runIdentity $ get f v) a)) ac
            Join (Conj fromField toField) (rel :: Relation o fv) _ ->
               do let keys = map (runIdentity . get fromField) dat
                  (agdat :: [(ViewTable o Identity, o)]) <- performViewQuery $ tableSieve (\tab -> _in (get toField tab) keys)
                  let agrMap = M.fromListWith (++) $ map ((runIdentity . get toField) *** (:[])) agdat
                  case rel of
                    ParentV      -> return $ map (\(r, v) -> (r, set field (fromMaybe (error "Field value not found") $ headMay =<< M.lookup (runIdentity $ get fromField r) agrMap) v)) ac
                    SingleChildV -> return $ map (\(r, v) -> (r, set field (fromMaybe (error "Field value not found") $ headMay =<< M.lookup (runIdentity $ get fromField r) agrMap) v)) ac
                    OptionChildV -> return $ map (\(r, v) -> (r, set field (headMay =<< M.lookup (runIdentity $ get fromField r) agrMap) v)) ac
                    MultiChildV  -> return $ map (\(r, v) -> (r, set field (fromMaybe [] $ M.lookup (runIdentity $ get fromField r) agrMap) v)) ac

addInsert :: InsertExpr i a -> InsertExpr i a -> InsertExpr i a
addInsert i i' | isDefault i' = i
               | otherwise    = i'

-- | Generic view inserting updating function, takes a list of viewable values and possible pre-filled data
saveView :: forall a. View a
         => Query Single Z (ViewTable a (InsertExpr Single)) -> [a] -> Db [(a, ViewTable a Identity)]
saveView ini vs =
  do let ViewQuery _ store fields = viewQuery :: ViewQuery a (ViewTable a)
         ini' :: [(a, Query Single Z (ViewTable a (InsertExpr Single)))]
         ini'   = map (id &&& (\v -> liftM (zip1 addInsert (store v)) ini)) vs
     -- Set field values and create/join parents
     qs <- foldrM' ini' fields $ \(ViewField field fType def) ac ->
              case (def, fType) of
                (NoDefault, Base f) -> 
                    return $ flip map ac $ \(a, q) ->
                               (a, modify f (addInsert (con' $ a |.| field)) `liftM` q)
                (NoDefault, Join (Conj fromField toField) ParentV CreateV) ->
                    forM ac $ \(a,q) ->
                             do [(_,x)] <- saveView (return defaultInsert) [a |.| field]
                                return ( a
                                       , do r <- q
                                            case isDefault (r |.| fromField) of 
                                              False -> return r
                                              True -> return $ fromField |->| (constant $ runIdentity $ x |.| toField) $ r
                                       )
                (NoDefault, Join (Conj fromField toField) ParentV RelateV) ->
                    return $ flip map ac $ \(a,q) ->
                                      ( a
                                      , do ins <- q
                                           case isDefault (ins |.| fromField) of 
                                             False -> return ins
                                             True -> do relPar <- table >>= filterQuery (a |.| field)
                                                        return $ fromField |->| (relPar |.| toField) $ ins
                                      )
                _ -> return ac
     -- Insert rows
     rs <- fmap concat $ forM qs $ \(a, q) -> fmap (map ((,) a)) $ insert' q
     -- Create children
     forM_ fields $ \(ViewField field fType _) ->
       forM_ rs $ \(a, r) ->
         let value   = get field a
         in case fType of
             (Join (Conj fromField toField) pType CreateV) ->
               let insertE = toField |->| (constant $ runIdentity $ get fromField r) $ defaultInsert
               in case pType of
                    SingleChildV -> saveView (return insertE) [value] >> return ()
                    OptionChildV -> saveView (return insertE) (maybeToList value) >> return ()
                    MultiChildV  -> saveView (return insertE) value >> return ()
                    _            -> return ()
             _ -> return ()
     return rs


-- | Generic view updating function
performViewUpdate :: forall a. (View a, Queryable a)
           => (ViewTable a (SingleExpr Z) -> Query Single Z (ViewTable a UpdateExpr))
           -> a -> Db [ViewTable a Identity]
performViewUpdate fun a =
  do let ViewQuery _ store fields = viewQuery :: ViewQuery a (ViewTable a)

         addUpdate :: (MeetAggr i Single ~ Single) => InsertExpr i x -> UpdateExpr x -> UpdateExpr x
         addUpdate i i' | isIgnore i' = cast i
                        | otherwise   = i'

         fun' = fmap (fmap $ zip1 addUpdate (store a)) fun
     -- Set field values and update/join parents
     q <- foldrM' fun' fields $ \(ViewField field fType def) q' ->
              case (def, fType) of
                (NoDefault, Base f) -> return $ fmap (fmap $ f |->| (constant $ a |.| field)) q'
                (NoDefault, Join (Conj fromField toField) ParentV RelateV) ->
                    return $ \i ->
                                do upd    <- q' i
                                   relPar <- table >>= filterQuery (a |.| field)
                                   return $ fromField |->| (relPar |.| toField) $ upd
                _ -> return q'
     -- Update rows
     update' q 


{-
baseView :: AbstractType a t => ViewQuery a t
baseView = ViewQuery id []
-}

-- | Query a table and marshal using View
queryView :: View a => Query i Z (ViewTable a (Expr i Z)) -> Db [a]
queryView = fmap (map snd) . performViewQuery

-- | Query all elements of a table
queryAll :: View a => Db [a]
queryAll = queryView table

-- | Insert a list of values with some default values pre-filled
insertViews' :: View a => Query Single Z (ViewTable a (InsertExpr Single)) -> [a] -> Db ()
insertViews' v vs = saveView v vs >> return ()

-- | Insert a list of value
insertViews :: View a => [a] -> Db ()
insertViews = insertViews' (return defaultInsert)

-- | Insert a 'Viewable' value into its corresponding table
insertView :: View a => a -> Db ()
insertView = insertViews . return

-- | Insert a 'Viewable' value with additional information
insertView' :: View a => Query Single Z (ViewTable a (InsertExpr Single)) -> a -> Db ()
insertView' q = insertViews' q . return

-- | Update an existing value
updateView' :: (QueryTable a ~ ViewTable a, View a, Queryable a) => a -> Db Int
updateView' a = fmap length $ performViewUpdate (\v -> filterQuery a v >> return emptyUpdate) a

-- | Update a value using its own reference. The field upon which Queryable depends can not be updated!
updateView :: (QueryTable a ~ ViewTable a, View a, Queryable a) => a -> Db Int
updateView a = fmap length $ performViewUpdate (\v -> filterQuery a v >> return emptyUpdate) a

-- | Update and if that fail, create
updateOrCreate :: (QueryTable a ~ ViewTable a, View a, Queryable a) => a -> Db ()
updateOrCreate a =
  do r <- updateView a
     when (r == 0) (insertView a)

data  Parent      = Parent     
data  SingleChild = SingleChild
data  OptionChild = OptionChild
data  MultiChild  = MultiChild 

data Create = Create
data Relate = Relate
data Ignore = Ignore

-- | This type class allows the construction of flexible views: a view which is dependent
-- on the type. 
class Eq v => FieldJoin t' x m c v where
  mkFieldJoin :: (Ord x, Convertible x SqlValue, Convertible SqlValue x) 
              => (t :><: t') x -> m -> c -> FieldType t v

instance FieldJoin t' x m c () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x Parent      Create () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x Parent      Relate () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x Parent      Ignore () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x SingleChild Create () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x SingleChild Relate () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x SingleChild Ignore () where mkFieldJoin _ _ _ = Empty

instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x m c x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x Parent Create x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x Parent Relate x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x Parent Ignore x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x SingleChild Create x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x SingleChild Relate x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x) 
      => FieldJoin t' x SingleChild Ignore x  where mkFieldJoin (Conj from _) _ _ = Base from

instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x Parent Create o where 
  mkFieldJoin cnj _ _ = Join cnj ParentV CreateV
instance (Convertible x SqlValue, Convertible SqlValue x
         , Queryable o, QueryTable o ~ ViewTable o
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x Parent Relate o where 
  mkFieldJoin cnj _ _ = Join cnj ParentV RelateV
instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x Parent Ignore o where 
  mkFieldJoin cnj _ _ = Join cnj ParentV IgnoreV

instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x SingleChild Create o where 
  mkFieldJoin cnj _ _ = Join cnj SingleChildV CreateV
instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x SingleChild Ignore o where 
  mkFieldJoin cnj _ _ = Join cnj SingleChildV IgnoreV

instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x OptionChild Create (Maybe o) where 
  mkFieldJoin cnj _ _ = Join cnj OptionChildV CreateV
instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x OptionChild Ignore (Maybe o) where 
  mkFieldJoin cnj _ _ = Join cnj OptionChildV IgnoreV

instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x MultiChild Create [o] where 
  mkFieldJoin cnj _ _ = Join cnj MultiChildV CreateV
instance (Convertible x SqlValue, Convertible SqlValue x
         , View o, t' ~ ViewTable o
         ) => FieldJoin t' x MultiChild Ignore [o] where 
  mkFieldJoin cnj _ _ = Join cnj MultiChildV IgnoreV

mkAbstractView :: Name -> Name -> Q [Dec]
mkAbstractView nm nm' =
  do TyConI d <- reify nm
     TyConI d' <- reify nm'
     mkAbstractView' d d'

mkAbstractView' :: Dec -> Dec -> Q [Dec]
mkAbstractView' (DataD _ rnm rpars _ _) (DataD _ anm _ _ _) =
    let ps = map getTV rpars
    in return [InstanceD []
                    (ConT (mkName "View") `AppT` (foldl AppT (ConT rnm) $ map VarT ps))
                    [ TySynInstD (mkName "ViewTable") [foldl AppT (ConT rnm) $ map VarT ps] $ foldl AppT (ConT anm) (map VarT ps)
                    , FunD (mkName "viewQuery")
                        [Clause [] (NormalB $ foldl AppE (ConE $ mkName "ViewQuery") 
                                    [ VarE $ mkName "fromAbstract"
                                    , VarE $ mkName "toAbstractInsert"
                                    , ListE []]) []]
                    ]
            ]
mkAbstractView' _ _ = error "Error in arguments to mkAbstractView'"

class Empty f a | f -> a where emptyData :: f -> a
instance Empty b c => Empty (a -> b) c where emptyData f = emptyData $ f undefined
instance a ~ b => Empty a b where emptyData = id

