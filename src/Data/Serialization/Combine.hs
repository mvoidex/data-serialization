module Data.Serialization.Combine (
    Iso(..), Combine(..)
    ) where

import Data.Iso

class Combine f where
    (.**.) :: f a -> f b -> f (a, b)
    (.++.) :: f a -> f b -> f (Either a b)
    (.+.) :: f a -> f a -> f a
    (<<>>) :: Iso a b -> f a -> f b
    (.:.) :: f b -> Iso a b -> f a
    (.*>>) :: f a -> (a -> f b) -> f (a, b)
    pures :: a -> f a
    fails :: f a
