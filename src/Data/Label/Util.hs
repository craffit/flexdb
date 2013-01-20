{-# LANGUAGE Rank2Types, TypeOperators #-}
module Data.Label.Util where

import Data.Label

type a :> b = forall f . a f :-> f b
data (:><:) a b x = Conj { firstJoin :: a :> x, secondJoin :: b :> x }

(>-<) :: (a :> x) -> (b :> x) -> (a :><: b) x
(>-<) = Conj