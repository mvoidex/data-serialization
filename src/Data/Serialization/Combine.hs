module Data.Serialization.Combine (
    Iso(..), Combine(..)
    ) where

data Iso a b = Iso (a -> b) (b -> a)

class Combine f where
    (.**.) :: f a -> f b -> f (a, b)
    (.++.) :: f a -> f b -> f (Either a b)
    (.+.) :: f a -> f a -> f a
    (<<>>) :: Iso a b -> f a -> f b
    (.*>>) :: f a -> (a -> f b) -> f (a, b)
    pures :: a -> f a
    fails :: f a
