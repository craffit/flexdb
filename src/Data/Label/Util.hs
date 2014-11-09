{-# LANGUAGE Rank2Types, TypeOperators, GADTs #-}
module Data.Label.Util where

import Data.Label

-- A synonym for abstract lenses
type a :> b = forall f . a f :-> f b
newtype a :>: b = ALens { aLens :: forall f . a f :-> f b }

-- A datastructure for 'joining' lenses
data (:><:) a b x = Conj { firstJoin :: a :> x, secondJoin :: b :> x }

(>-<) :: (a :> x) -> (b :> x) -> (a :><: b) x
(>-<) = Conj

flipJoin :: (a :><: b) x -> (b :><: a) x
flipJoin (Conj a b) = Conj b a

-- | A lens with existential result type
data Label a where
  Label :: a :> x -> Label a

-- | Lens with an unapplied subtype

data ULens f o = ULens { uLens :: f :-> o }

ulens :: (f -> o) -> ((o -> o) -> f -> f) -> ULens f o
ulens get mod = ULens $ lens get mod
