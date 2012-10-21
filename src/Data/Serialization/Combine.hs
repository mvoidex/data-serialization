{-# LANGUAGE TypeOperators #-}

module Data.Serialization.Combine (
    Iso(..), Combine(..),
    (:~:)(..)
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

infixr 0 :~:

data (:~:) l r a = Combined {
    combineLeft :: l a,
    combineRight :: r a }

instance (Combine l, Combine r) => Combine (l :~: r) where
    (Combined ll lr) .**. (Combined rl rr) = Combined (ll .**. rl) (lr .**. rr)
    (Combined ll lr) .++. (Combined rl rr) = Combined (ll .++. rl) (lr .++. rr)
    (Combined ll lr) .+. (Combined rl rr) = Combined (ll .+. rl) (lr .+. rr)
    iso <<>> (Combined l r) = Combined (iso <<>> l) (iso <<>> r)
    (Combined l r) .:. iso = Combined (l .:. iso) (r .:. iso)
    (Combined l r) .*>> f = Combined (l .*>> combineLeft . f) (r .*>> combineRight . f)
    pures x = Combined (pures x) (pures x)
    fails = Combined fails fails
