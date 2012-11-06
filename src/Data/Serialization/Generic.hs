{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, DefaultSignatures, FlexibleContexts, ConstraintKinds, UndecidableInstances #-}

module Data.Serialization.Generic (
    -- * Generic serialization classes
    GenericCombine(..),
    GenericEncode(..), 
    GenericDecode(..),
    Serializable(..), GenericSerializable(..),
    -- * Functions to serialize parts of @Generics@
    dat, dat_, datT,
    ctor, ctor_, ctorT,
    stor, stor_, storT,

    module Data.Serialization.Combine
    ) where

import Control.Applicative
import Data.Function (fix)
import GHC.Generics

import Data.Serialization.Combine

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
    default ser :: (GenIsoDerivable (GenericSerializable f) a) => f a
    ser = gser .:. giso

-- | Generic serializable
class GenericCombine f => GenericSerializable f a where
    gser :: f a

instance (GenericSerializable f a, Datatype c) => GenericSerializable f (Data c a) where
    gser = dat_ gser

instance (GenericSerializable f a, Constructor c) => GenericSerializable f (Ctor c a) where
    gser = ctor_ gser

instance (GenericCombine f, Serializable f a, Selector c) => GenericSerializable f (Stor c a) where
    gser = stor_ ser

instance (GenericSerializable f a, GenericSerializable f b) => GenericSerializable f (a, b) where
    gser = gser .*. gser

instance (GenericSerializable f a, GenericSerializable f b) => GenericSerializable f (Either a b) where
    gser = gser .+. gser

-- | Serializer of generic @Datatype@ with explicit name
dat :: (GenericCombine f, Datatype c) => String -> f a -> f (Data c a)
dat name s = genericData name s .:. Iso unData Data

-- | Serializer of generic @Datatype@ with @dataName@
dat_ :: (GenericCombine f, Datatype c) => f a -> f (Data c a)
dat_ s = fix $ \r -> dat (dataName $ dummy r) s where
    dummy :: GenericCombine f => f (Data c a) -> Data c a
    dummy _ = undefined

-- | Serializer of @Datatype@, providing @dataName@ to your serializer
datT :: (GenericCombine f, Datatype c) => (String -> f a) -> f (Data c a)
datT s = fix $ \r -> dat (dataName $ dummy r) (s (dataName $ dummy r)) where
    dummy :: GenericCombine f => f (Data c a) -> Data c a
    dummy _ = undefined

-- | Serializer of generic @Constructor@ with explicit name
ctor :: (GenericCombine f, Constructor c) => String -> f a -> f (Ctor c a)
ctor name s = genericCtor name s .:. Iso unCtor Ctor

-- | Serializer of generic @Contructor@ with @ctorName@
ctor_ :: (GenericCombine f, Constructor c) => f a -> f (Ctor c a)
ctor_ s = fix $ \r -> ctor (ctorName $ dummy r) s where
    dummy :: GenericCombine f => f (Ctor c a) -> Ctor c a
    dummy _ = undefined

-- | Serializer of @Constructor@, providing @ctorName@ to your serializer
ctorT :: (GenericCombine f, Constructor c) => (String -> f a) -> f (Ctor c a)
ctorT s = fix $ \r -> ctor (ctorName $ dummy r) (s (ctorName $ dummy r)) where
    dummy :: GenericCombine f => f (Ctor c a) -> Ctor c a
    dummy _ = undefined

-- | Serializer of generic @Selector@ with explicit name
stor :: (GenericCombine f, Selector c) => String -> f a -> f (Stor c a)
stor name s = genericStor name s .:. Iso unStor Stor

-- | Serializer of generic @Selector@ with @storName@
stor_ :: (GenericCombine f, Selector c) => f a -> f (Stor c a)
stor_ s = fix $ \r -> stor (storName $ dummy r) s where
    dummy :: GenericCombine f => f (Stor c a) -> Stor c a
    dummy _ = undefined

-- | Serializer of @Constructor@, providing @ctorName@ to your serializer
storT :: (GenericCombine f, Selector c) => (String -> f a) -> f (Stor c a)
storT s = fix $ \r -> stor (storName $ dummy r) (s (storName $ dummy r)) where
    dummy :: GenericCombine f => f (Stor c a) -> Stor c a
    dummy _ = undefined
