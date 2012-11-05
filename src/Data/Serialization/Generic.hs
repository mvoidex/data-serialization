{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, DefaultSignatures, FlexibleContexts #-}

module Data.Serialization.Generic (
    -- * Generic serialization classes
    GenericCombine(..),
    GenericEncode(..), 
    GenericDecode(..),
    Serializable(..), GenericSerializable(..),
    -- * Functions to serialize parts of @Generics@
    dat, dat_,
    ctor, ctor_,
    stor, stor_,
    field, field_,
    val,
    (.**.),
    (.++.),

    module Data.Serialization.Combine
    ) where

import Control.Applicative
import Data.Function (fix)
import GHC.Generics

import Data.Serialization.Combine

infixr 7 .**.
infixr 6 .++.

-- | Generic serialize operations for data, constructor and selector
class Combine f => GenericCombine f where
    -- | Serialize data by its name and sub-serializer
    genericData :: String -> f a -> f a
    -- | Serialize contructor by its name and sub-serializer
    genericCtor :: String -> f a -> f a
    -- | Serialize selector by its name and sub-serializer
    genericStor :: String -> f a -> f a

    genericData _ = id
    genericCtor _ = id
    genericStor _ = id

-- | Encoder for constructor and selectors, used to provides it's named to serializer
-- by default just ignores names
class GenericEncode m where
    encodeData :: String -> (a -> m ()) -> a -> m ()
    encodeCtor :: String -> (a -> m ()) -> a -> m ()
    encodeStor :: String -> (a -> m ()) -> a -> m ()

    encodeData _ = id
    encodeCtor _ = id
    encodeStor _ = id

instance (Monad m, GenericEncode m) => GenericCombine (Encoding m) where
    genericData s ~(Encoding e) = Encoding $ encodeData s e
    genericCtor s ~(Encoding e) = Encoding $ encodeCtor s e
    genericStor s ~(Encoding e) = Encoding $ encodeStor s e

-- | Decoder for constructors and selectors, provides it's names to deserializer
-- By default sets names to empty strings
class (Functor m) => GenericDecode m where
    decodeData :: String -> m a -> m a
    decodeCtor :: String -> m a -> m a
    decodeStor :: String -> m a -> m a

    decodeData _ = id
    decodeCtor _ = id
    decodeStor _ = id

instance (Alternative m, GenericDecode m) => GenericCombine (Decoding m) where
    genericData s ~(Decoding d) = Decoding $ decodeData s d
    genericCtor s ~(Decoding d) = Decoding $ decodeCtor s d
    genericStor s ~(Decoding d) = Decoding $ decodeStor s d

-- | Serializable class
class (Combine f) => Serializable f a where
    ser :: f a
    default ser :: (Generic a, GenericSerializable f (Rep a)) => f a
    ser = gser .:. giso

-- | Generic serializable
class (GenericCombine f) => GenericSerializable f a where
    gser :: f (a p)

instance (GenericCombine f, Serializable f a) => GenericSerializable f (K1 i a) where
    gser = val ser

instance (GenericSerializable f a, Datatype c) => GenericSerializable f (M1 D c a) where
    gser = dat_ gser

instance (GenericSerializable f a, Constructor c) => GenericSerializable f (M1 C c a) where
    gser = ctor_ gser

instance (GenericSerializable f a, Selector c) => GenericSerializable f (M1 S c a) where
    gser = stor_ gser

instance (GenericSerializable f a, GenericSerializable f b) => GenericSerializable f (a :*: b) where
    gser = gser .**. gser

instance (GenericSerializable f a, GenericSerializable f b) => GenericSerializable f (a :+: b) where
    gser = gser .++. gser

-- | Serializer of generic @Datatype@ with explicit name
dat :: (GenericCombine f, Datatype c) => String -> f (a p) -> f (M1 D c a p)
dat name s = genericData name s .:. Iso unM1 M1

-- | Serializer of generic @Datatype@ with @datatypeName@
dat_ :: (GenericCombine f, Datatype c) => f (a p) -> f (M1 D c a p)
dat_ s = fix $ \r -> dat (datatypeName $ dummy r) s where
    dummy :: GenericCombine f => f (M1 D c a p) -> M1 D c a p
    dummy _ = undefined

-- | Serializer of generic @Constructor@ with explicit name
ctor :: (GenericCombine f, Constructor c) => String -> f (a p) -> f (M1 C c a p)
ctor name s = genericCtor name s .:. Iso unM1 M1

-- | Serializer of generic @Contructor@ with @conName@
ctor_ :: (GenericCombine f, Constructor c) => f (a p) -> f (M1 C c a p)
ctor_ s = fix $ \r -> ctor (conName $ dummy r) s where
    dummy :: GenericCombine f => f (M1 C c a p) -> M1 C c a p
    dummy _ = undefined

-- | Serializer of generic @Selector@ with explicit name
stor :: (GenericCombine f, Selector c) => String -> f (a p) -> f (M1 S c a p)
stor name s = genericStor name s .:. Iso unM1 M1

-- | Serializer of generic @Selector@ with @selName@
stor_ :: (GenericCombine f, Selector c) => f (a p) -> f (M1 S c a p)
stor_ s = fix $ \r -> stor (selName $ dummy r) s where
    dummy :: GenericCombine f => f (M1 S c a p) -> M1 S c a p
    dummy _ = undefined

-- | @stor@ with @val@
field :: (GenericCombine f, Selector c) => String -> f a -> f (M1 S c (K1 i a) p)
field name = stor name . val

-- | @stor_@ with @val@
field_ :: (GenericCombine f, Selector c) => f a -> f (M1 S c (K1 i a) p)
field_ = stor_ . val

-- | Serializer of @K1@ wrapper
val :: (Combine f) => f a -> f (K1 i a p)
val s = s .:. Iso unK1 K1

-- | Serializer of generic @:*:@
(.**.) :: Combine f => f (a p) -> f (b p) -> f ((a :*: b) p)
l .**. r = (l .*. r) .:. Iso de en where
    de (x :*: y) = (x, y)
    en (x, y) = (x :*: y)

-- | Serializer of generic @:+:@
(.++.) :: Combine f => f (a p) -> f (b p) -> f ((a :+: b) p)
l .++. r = (l .+. r) .:. Iso de en where
    de (L1 x) = Left x
    de (R1 y) = Right y
    en (Left x) = L1 x
    en (Right y) = R1 y
