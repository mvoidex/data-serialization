module Data.Serialization.Combine (
    Iso(..), Combine(..)
    ) where

import Data.Iso

infixr 6 .++.
infixr 7 .**.
infixr 6 .+.
infixl 2 .:.
infixl 8 .*>>

class Combine f where
    (.**.) :: f a -> f b -> f (a, b)
    (.++.) :: f a -> f b -> f (Either a b)
    (.+.) :: f a -> f a -> f a
    (<<>>) :: Iso a b -> f a -> f b
    (.:.) :: f b -> Iso a b -> f a
    (.*>>) :: f a -> (a -> f b) -> f (a, b)
    pures :: a -> f a
    fails :: f a
