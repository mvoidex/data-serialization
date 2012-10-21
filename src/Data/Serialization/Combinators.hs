module Data.Serialization.Combinators (
    (.**),
    (**.),
    (.?.),
    try,
    (.-.),
    narrow, list,
    many, many1, times,
    literal, literals,
    intercalated, intercalated1,
    (<~>), (<~~>)
    ) where

import Control.Applicative hiding (many)
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Serialization.Combine
import Data.Serialization.Serializable

infixl 8 .**
infixr 8 **.
infixr 1 .?.
infixl 6 .-.

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

-- | Try serialize
try :: (Combine f) => f a -> f (Maybe a)
try p = justp .+. nothingp where
    justp = isJust .?. ((Iso Just (fromMaybe (error "Impossible happened"))) <<>> p)
    nothingp = pures Nothing

-- | Serialize left if right fails
(.-.) :: (Combine f) => f a -> f a -> f a
l .-. r = dropTry <<>> (try r .*>> l') where
    l' Nothing = l
    l' (Just _) = fails
    dropTry :: Iso (Maybe a, a) a
    dropTry = Iso snd (Just &&& id)

-- | Apply function on both encode and decode
narrow :: (Combine f) => (a -> a) -> f a -> f a
narrow f p = (Iso f f) <<>> p

-- | List iso
list :: Iso (a, [a]) [a]
list = Iso (uncurry (:)) (head &&& tail)

-- | Zero or more
many :: (Combine f) => f a -> f [a]
many p = ps .+. pz where
    ps = list <<>> (p .**. many p)
    pz = pures []

-- | One or more
many1 :: (Combine f) => f a -> f [a]
many1 p = list <<>> (p .**. many p)

-- | Serialize N values
times :: (Combine f) => Int -> f a -> f [a]
times 0 p = pures []
times n p = list <<>> (p .**. times (n - 1) p)

-- | Literal
literal :: (Eq a, Combine f) => f a -> a -> f ()
literal t v = (Iso (const ()) (const v)) <<>> ((== v) .?. t)

-- | List of literals
literals :: (Eq a, Combine f) => f a -> [a] -> f ()
literals p ls = literal (times (length ls) p) ls

-- | List of values intercalated with literals
intercalated :: (Combine f) => f a -> f () -> f [a]
intercalated p l = ps .+. pz where
    ps = list <<>> (p .**. many (l **. p))
    pz = pures []

-- | One or more intercalated
intercalated1 :: (Combine f) => f a -> f () -> f [a]
intercalated1 p l = list <<>> (p .**. many (l **. p))

-- | Streaming parsers
(<~>)
    :: (Monad sm, Alternative sm, Monad dm, Alternative dm, Serialization sm' i, Deserialization dm' i)
    => Serializable s sm dm i
    -> Serializable i sm' dm' a
    -> Serializable s sm dm a
i <~> o = i <~~> recode o

-- | Like streaming, but accepts 'Convertible'
(<~~>) :: (Monad sm, Alternative sm, Monad dm, Alternative dm) =>
    Serializable s sm dm i -> Convertible a i -> Serializable s sm dm a
i <~~> c = (Iso fromRight Right) <<>> (isRight .?. (c' <<>> i')) where
    i' = isRight .?. (Iso Right fromRight <<>> i)
    c' = Iso (>>= convertFrom c) (>>= convertTo c)

    isRight (Right _) = True
    isRight _ = False
    fromRight (Right v) = v
    fromRight _ = error "Impossible happened"
