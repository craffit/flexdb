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

import Data.Foldable hiding (concat, foldl, foldr)
import Data.Maybe
import Data.Record.Abstract
import Data.Zippable1
import Data.Foldable1

import DB.Flex.Table
import DB.Flex.Monad
import DB.Flex.Record
import DB.Flex.Query.Typed
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Util

import Safe

-- | Main type class for describing a Haskell 'view' on a database table. This view can aggregate data from
-- others tables and specifies the way in which the data structure can be saved.

class (TableDef (ViewTable a), Eq a) => View a where
  -- | The table for which the view is defined
  type ViewTable a :: (* -> *) -> *
  viewQuery :: ViewQuery a (ViewTable a)

-- | A view query is just a list of 'Fields' which can be specified.
data ViewQuery a t =
  ViewQuery
    { emptyView  :: a
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
  Base  :: DBType x => t :> x -> FieldType t x
  Join  :: (View o, Ord x, DBType x) =>
         { fieldJoin :: (t :><: ViewTable o) x
         , relation  :: Relation o fv
         , creation  :: Creation
         } -> FieldType t fv

-- | Description of join relations. Describes what the remote table is to the current table.
data Relation o v where
  ParentV      :: Relation o o
  SingleChildV :: Relation o o
  OptionChildV :: Relation o (Maybe o)
  MultiChildV  :: Relation o [o]

-- | Represents how joined tables should be handled during insertion and update
data Creation where
  Create :: Creation
  Relate :: Creation
  Ignore :: Creation

data Default = Default | NoDefault

-- | Convenient combinators for constructing views

infix 5 |+, |=, |?

(|+) :: a :-> v -> FieldType t v -> ViewField a t
to |+ aggr = ViewField to aggr NoDefault

(|=) :: DBType v => a :-> v -> t :> v -> ViewField a t
to |= from = ViewField to (Base from) NoDefault

(|?) :: DBType v => a :-> v -> t :> v -> ViewField a t
to |? from = ViewField to (Base from) Default

-- | foldrM with arguments permuted
foldrM' :: Monad m => b -> [a] -> (a -> b -> m b) -> m b
foldrM' b l f = foldrM f b l

-- | Query a view value
viewFilter :: forall a i l. View a => a -> ViewTable a (SingleExpr l) -> Query i l ()
viewFilter a q =
  let ViewQuery _ fields = viewQuery :: ViewQuery a (ViewTable a)
      viewQ = (\f -> foldr f (return defaultInsert) fields) $ \(ViewField field fType _) q' ->
                  case fType of
                      Base f -> fmap (f |->>| a |.| field) q'
                      Join (Conj fromField toField) ParentV _ ->
                                  do r    <- q'
                                     relPar <- table >>= filterView (a |.| field)
                                     return $ fromField |->| relPar |.| toField $ r
                      _ -> q'
  in viewQ >>= (\e -> filterRecord e q)

filterView :: forall a i l. View a => a -> ViewTable a (SingleExpr l) -> Query i l (ViewTable a (SingleExpr l))
filterView a q = viewFilter a q >> return q

-- | Generic function for querying views. Will aggregate all appropriate data as specified in the viewQuery from View
performViewQuery :: forall a i. View a => Query i Z (ViewTable a (Expr i Z)) -> Db [(ViewTable a Identity, a)]
performViewQuery q =
   do dat <- query' q
      let ViewQuery emp fields = viewQuery :: ViewQuery a (ViewTable a)
          empt   = fmap (flip (,) emp) dat
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

addInsert :: InsertExpr i l a -> InsertExpr i l a -> InsertExpr i l a
addInsert i i' | isDefault i' = i
               | otherwise    = i'

-- | Generic view inserting updating function, takes a list of viewable values and possible pre-filled data
saveView :: forall a. View a
         => [(Query Single Z (ViewTable a (InsertExpr Single Z)), a)] -> Db [[ViewTable a Identity]]
saveView inf =
  do let ViewQuery _ fields = viewQuery :: ViewQuery a (ViewTable a)
         (inis, vs) = unzip inf
     -- Set field values and create/join parents
     qs' <- foldrM' inis fields $ \(ViewField field fType def) qs ->
              case (def, fType) of
                (NoDefault, Base f) ->
                    return $ zipWith (\a -> liftM $ modify f (addInsert (con' $ a |.| field))) vs qs
                (NoDefault, Join (Conj fromField toField) ParentV Create) ->
                    -- Note: parents will always override default query.
                    do ps <- map head `liftM` saveView (map ((,) (return defaultInsert) . get field) vs)
                       return $ zipWith (\p -> liftM $ fromField |->>| runIdentity (p |.| toField)) ps qs
                (NoDefault, Join (Conj fromField toField) ParentV Relate) ->
                    return $ zipWith (\a q ->
                                        do ins <- q
                                           case isDefault (ins |.| fromField) of
                                             False -> return ins
                                             True -> do relPar <- table >>= filterView (a |.| field)
                                                        return $ fromField |->| (relPar |.| toField) $ ins
                                     ) vs qs
                _ -> return qs
     -- Insert rows
     rs <- insertMany' qs'
     -- Create children
     forM_ fields $ \(ViewField field fType _) ->
            case fType of
             (Join (Conj fromField toField) (pType :: Relation o o') Create) ->
               let vals :: [o']
                   vals = map (get field) vs
                   listVals :: [[o]]
                   listVals = case pType of
                                SingleChildV -> map (:[]) vals
                                OptionChildV -> map maybeToList vals
                                MultiChildV  -> vals
                                _            -> []
                   insertE :: [[Query Single Z (ViewTable o (InsertExpr Single Z))]]
                   insertE = map (map $ \r -> return $ toField |->>| runIdentity (r |.| fromField) $ defaultInsert) rs
               in saveView (concat $ zipWith (liftM2 (,)) insertE listVals) >> return ()
             _ -> return ()
     return rs


-- | Generic view updating function
performViewUpdate :: forall a. View a
           => (ViewTable a (SingleExpr Z) -> Query Single Z (ViewTable a UpdateExpr))
           -> a -> Db [ViewTable a Identity]
performViewUpdate fun a =
  do let ViewQuery _ fields = viewQuery :: ViewQuery a (ViewTable a)
{-     fun' <- foldrM' fun fields $ \(ViewField field fType def) q' ->
              case (def, fType) of
                (NoDefault, Join (Conj fromField toField) ParentV Relate) ->
                _ -> return q'
-}
     -- Set field values and update/join parents
     q <- foldrM' fun fields $ \(ViewField field fType def) q' ->
              case (def, fType) of
                (NoDefault, Base f) -> return $ fmap (fmap $ f |->>| a |.| field) q'
                (NoDefault, Join (Conj fromField toField) ParentV Relate) ->
                    return $ \i ->
                                do upd    <- q' i
                                   relPar <- table >>= filterView (a |.| field)
                                   return $ fromField |->| (relPar |.| toField) $ upd
                _ -> return q'
     -- Update rows
     update' q

viewRecord :: View a => a -> Query i l (ViewTable a (SingleExpr l))
viewRecord a = table >>= filterView a

-- | Query a table and marshal using View
queryView :: View a => Query i Z (ViewTable a (Expr i Z)) -> Db [a]
queryView = fmap (map snd) . performViewQuery

-- | Query all elements of a table
queryAll :: View a => Db [a]
queryAll = queryView table

-- | Insert a list of values with some default values pre-filled
insertViews' :: View a => Query Single Z (ViewTable a (InsertExpr Single Z)) -> [a] -> Db ()
insertViews' v vs = saveView (map ((,) v) vs) >> return ()

-- | Insert a list of value
insertViews :: View a => [a] -> Db ()
insertViews = insertViews' (return defaultInsert)

-- | Insert a 'Viewable' value into its corresponding table
insertView :: View a => a -> Db ()
insertView = insertViews . return

-- | Insert a 'Viewable' value with additional information
insertView' :: View a => Query Single Z (ViewTable a (InsertExpr Single Z)) -> a -> Db ()
insertView' q = insertViews' q . return

-- | Update an existing value
updateView' :: View a => a -> Db Int
updateView' a = fmap length $ performViewUpdate (\v -> filterView a v >> return emptyUpdate) a

-- | Update a value using its own reference.
updateView :: View a => a -> a -> Db Int
updateView a a' = fmap length $ performViewUpdate (\v -> filterView a v >> return emptyUpdate) a'

-- | Update and if that fail, create
updateOrCreate :: View a => a -> Db ()
updateOrCreate a =
  do r <- updateView' a
     when (r == 0) (insertView a)

data  Parent      = Parent
data  SingleChild = SingleChild
data  OptionChild = OptionChild
data  MultiChild  = MultiChild

-- | This type class allows the construction of flexible views: a view which is dependent
-- on the type.
class Eq v => FieldJoin t' x m v where
  mkFieldJoin :: (Ord x, DBType x)
              => (t :><: t') x -> m -> Creation -> FieldType t v

instance FieldJoin t' x m () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x Parent      () where mkFieldJoin _ _ _ = Empty
instance FieldJoin t' x SingleChild () where mkFieldJoin _ _ _ = Empty

instance (Convertible x SqlValue, Eq x)
      => FieldJoin t' x m x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x)
      => FieldJoin t' x Parent x  where mkFieldJoin (Conj from _) _ _ = Base from
instance (Convertible x SqlValue, Eq x)
      => FieldJoin t' x SingleChild x  where mkFieldJoin (Conj from _) _ _ = Base from

instance (DBType x, View o, t' ~ ViewTable o) => FieldJoin t' x Parent o where
  mkFieldJoin cnj _ = Join cnj ParentV
instance (DBType x, View o, t' ~ ViewTable o) => FieldJoin t' x SingleChild o where
  mkFieldJoin cnj _ = Join cnj SingleChildV
instance (DBType x, View o, t' ~ ViewTable o) => FieldJoin t' x OptionChild (Maybe o) where
  mkFieldJoin cnj _ = Join cnj OptionChildV
instance (DBType x, View o, t' ~ ViewTable o) => FieldJoin t' x MultiChild [o] where
  mkFieldJoin cnj _ = Join cnj MultiChildV


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
                                    [ VarE (mkName "emptyData") `AppE` ConE (mkName $ nameBase rnm)
                                    , VarE (mkName "abstractViewFields") ]) []]
                    ]
              ]
mkAbstractView' _ _ = error "Error in arguments to mkAbstractView'"

abstractViewFields :: (Record a, AbstractType v a) => [ViewField v a]
abstractViewFields = collect mkField $ zip1 Tup1 recordFields (zip1 Tup1 concreteLenses abstractLenses)
  where mkField (Tup1 Field (Tup1 cl (ALens al))) = ViewField cl (Base al) NoDefault

class Empty f a | f -> a where emptyData :: f -> a
instance Empty b c => Empty (a -> b) c where emptyData f = emptyData $ f undefined
instance a ~ b => Empty a b where emptyData = id

