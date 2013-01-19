{-# LANGUAGE Rank2Types, TypeOperators #-}
module Data.Label.Util where

import Data.Label

type a :> b = forall f . a f :-> f b
data (:><:) a b x = Conj (a :> x) (b :> x)

(>-<) :: (a :> x) -> (b :> x) -> (a :><: b) x
(>-<) = Conj