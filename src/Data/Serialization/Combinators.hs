{-# LANGUAGE FlexibleContexts #-}

module Data.Serialization.Combinators (
    (.*),
    (*.),
    try,
    (.-.),
    narrow, list,
    many, many1, times,
    literal, literals,
    intercalated, intercalated1,
    (<~>), (.~.),

    module Data.Serialization.Combine
    ) where

import Control.Applicative hiding (many)
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Serialization.Combine
import Data.Serialization.Codec

infixl 8 .*
infixr 8 *.
infixl 6 .-.
infixl 2 .~.

-- | Ignore right
(.*) :: (Combine f) => f a -> f () -> f a
l .* r = l .*. r .:. (Iso (\v -> (v, ())) fst)

-- | Ignore left
(*.) :: (Combine f) => f () -> f a -> f a
l *. r = l .*. r .:. (Iso (\v -> ((), v)) snd)

-- | Try serialize
try :: (CombineM f) => f a -> f (Maybe a)
try p = justp .|. nothingp where
    justp = isJust .?. p .:. (Iso (fromMaybe (error "Impossible happened")) Just)
    nothingp = pures Nothing

-- | Serialize left if right fails
(.-.) :: (CombineM f) => f a -> f a -> f a
l .-. r = try r .>> (maybe l (const $ fails "")) .:. dropTry where
    dropTry :: Iso a (Maybe a, a)
    dropTry = Iso (Just &&& id) snd

-- | Apply function on both encode and decode
narrow :: (Combine f) => (a -> a) -> f a -> f a
narrow f p = p .:. Iso f f

-- | List iso
list :: Iso [a] (a, [a])
list = Iso (head &&& tail) (uncurry (:))

-- | Zero or more
many :: (CombineM f) => f a -> f [a]
many p = ps .|. pz where
    ps = (not . null) .?. p .*. many p .:. list
    pz = pures []

-- | One or more
many1 :: (CombineM f) => f a -> f [a]
many1 p = p .*. many p .:. list

-- | Serialize N values
times :: (CombineM f) => Int -> f a -> f [a]
times 0 p = pures []
times n p = p .*. times (n - 1) p .:. list

-- | Literal
literal :: (Eq a, CombineM f) => f a -> a -> f ()
literal t v = ((== v) .?. t) .:. Iso (const v) (const ())

-- | List of literals
literals :: (Eq a, CombineM f) => f a -> [a] -> f ()
literals p ls = literal (times (length ls) p) ls

-- | List of values intercalated with literals
intercalated :: (CombineM f) => f a -> f () -> f [a]
intercalated p l = ps .|. pz where
    ps = p .*. many (l *. p) .:. list
    pz = pures []

-- | One or more intercalated
intercalated1 :: (CombineM f) => f a -> f () -> f [a]
intercalated1 p l = p .*. many (l *. p) .:. list

-- | Streaming parsers
(<~>)
    :: (Serializer sm' i, Deserializer dm' i, CombineM (Codec s sm dm))
    => Codec s sm dm i
    -> Codec i sm' dm' a
    -> Codec s sm dm a
i <~> o = i .~. recode o

-- | Like streaming, but accepts 'Convertible'
(.~.) :: (CombineM f) => f i -> Convertible a i -> f a
i .~. c = (isRight .?. (i' .:. c')) .:. Iso Right fromRight where
    i' = isRight .?. i .:. Iso fromRight Right
    c' = Iso (>>= convertTo c) (>>= convertFrom c)

    isRight = either (const False) (const True)
    fromRight = either (const $ error "Impossible happened") id
