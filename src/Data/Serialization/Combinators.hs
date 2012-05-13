module Data.Serialization.Combinators (
    (.**),
    (**.),
    (.?.),
    literal
    ) where

import Control.Applicative
import Control.Monad
import Data.Serialization.Combine
import Data.Serialization.Serializable

-- | Ignore right
(.**) :: (Combine f) => f a -> f () -> f a
l .** r = (Iso fst (\v -> (v, ()))) <<>> (l .**. r)

-- | Ignore left
(**.) :: (Combine f) => f () -> f a -> f a
l **. r = (Iso snd (\v -> ((), v))) <<>> (l .**. r)

-- | Check predicate
(.?.) :: (Combine f) => (a -> Bool) -> f a -> f a
p .?. s = (Iso fst (\v -> (v, ()))) <<>> (s .*>> check) where
    check v = if p v then pures () else fails

-- | Literal
literal
    :: (Eq a, Monad sm, Applicative sm, Alternative sm, Monad dm, Applicative dm, Alternative dm)
    => Serializable s sm dm a
    -> a
    -> Serializable s sm dm ()
literal t v = (Iso (const ()) (const v)) <<>> ((== v) .?. t)
