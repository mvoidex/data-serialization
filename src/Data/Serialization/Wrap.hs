{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DefaultSignatures, TypeFamilies #-}

-- | Module provides helpers to implement serializer as simple as possible
--
-- Suppose you want to write serializer to/from list of strings. Wrap @EncodeTo [String] a@ and @DecodeFrom [String] a@ with new types and make empty intances (all you need is derived automatically)
--
-- >newtype ToCSV a = ToCSV { toCSV :: EncodeTo [String] a }
-- >    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Generic)
-- >
-- >instance GenericEncode ToCSV
-- >instance Serializer ToCSV [String]
-- >
-- >newtype FromCSV a = FromCSV { fromCSV :: DecodeFrom [String] a }
-- >    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Generic)
-- >
-- >-- Implement functions to collect metadata, default implementation just throws it away
-- >instance GenericDecode FromCSV
-- >instance Deserializer FromCSV [String]
--
-- And that's all. Now write primitive serializers and use.
--
-- >showcsv :: (Show a) => Encoding ToCSV a
-- >showcscv = encodePart $ return . return . show
-- >
-- >readcsv :: (Read a) => Decoding FromCSV a
-- >readcsv = decodePart f where
-- >    f [] = Left \"EOF\"
-- >    f (c:cs) = case reads c of
-- >        [(v, \"\")] -> Right (v, cs)
-- >        _ -> Left \"Can't\ read value"
--
module Data.Serialization.Wrap (
    -- * Serializer classes
    Serializer(..), Deserializer(..),
    -- * Types to wrap over
    EncodeTo, DecodeFrom,
    -- * Wrap classes
    WrappedType,
    Wrapper(..),
    -- * Helper functions
    encodePart, decodePart,
    encodeTo, encodeTail,
    decodeFrom, decodeEof, decodeTail,
    Hint(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State
import Data.Monoid
import GHC.Generics

import Data.Serialization.Combine
import Data.Serialization.Generic

-- | Derive to support serialization for @Encoding@.
class (GenericEncode sm, Monad sm, Applicative sm, Alternative sm) => Serializer sm s where
    serialize :: sm () -> Either String s
    serializeTail :: s -> sm ()

    default serialize :: (Monoid s, Wrapper t, t ~ sm (), WrappedType (IsoRep t) ~ EncodeTo s ()) => t -> Either String s
    serialize = encodeTo . unwrap
    default serializeTail :: (Monoid s, Wrapper t, t ~ sm (), WrappedType (IsoRep t) ~ EncodeTo s ()) => s -> t
    serializeTail = wrap . encodeTail

-- | Derive to support deserialization for @Decoding@
class (GenericDecode dm, Monad dm, Applicative dm, Alternative dm) => Deserializer dm s where
    deserialize :: dm a -> s -> Either String a
    deserializeEof :: Hint s -> dm ()
    deserializeTail :: dm s

    default deserialize :: (Monoid s, Eq s, Wrapper t, t ~ dm a, WrappedType (IsoRep t) ~ DecodeFrom s a) => t -> s -> Either String a
    deserialize = decodeFrom . unwrap
    default deserializeEof :: (Monoid s, Eq s, Wrapper t, t ~ dm (), WrappedType (IsoRep t) ~ DecodeFrom s ()) => Hint s -> t
    deserializeEof = wrap . decodeEof
    default deserializeTail :: (Monoid s, Eq s, Wrapper t, t ~ dm s, WrappedType (IsoRep t) ~ DecodeFrom s s) => t
    deserializeTail = wrap decodeTail

-- | Wrap this with newtype and derive from @GenericEncode@ and @Serializer@
type EncodeTo s a = WriterT s (Either String) a

-- | Wrap this with newtype and derive from @GenericDecode@ and @Deserializer@
type DecodeFrom s a = ErrorT String (State s) a

-- | Helper function to produce some output
encodePart :: (Monoid s, Wrapper t, t ~ c (), WrappedType (IsoRep t) ~ EncodeTo s ()) => (a -> Either String s) -> Encoding c a
encodePart f = Encoding $ wrap . either (lift . Left) tell . f

-- | Helper function to consume some input
decodePart :: (Monoid s, Eq s, Wrapper t, t ~ c a, WrappedType (IsoRep t) ~ DecodeFrom s a) => (s -> Either String (a, s)) -> Decoding c a
decodePart f = Decoding $ wrap $ do
    s <- lift get
    if s == mempty
        then throwError "EOF"
        else do
            (v, s') <- ErrorT $ return $ f s
            lift $ put s'
            return v

-- | Helper for @serialize@
encodeTo :: (Monoid s) => EncodeTo s () -> Either String s
encodeTo = execWriterT

-- | Helper for @serializeTail@
encodeTail :: (Monoid s) => s -> EncodeTo s ()
encodeTail = tell

-- | Helper for @deserialize@
decodeFrom :: DecodeFrom s a -> s -> Either String a
decodeFrom d = evalState (runErrorT d)

-- | Helper for @deserializeEof@
decodeEof :: (Monoid s, Eq s) => Hint s -> DecodeFrom s ()
decodeEof _ = do
    s <- lift get
    when (s /= mempty) $ throwError "EOF expected"

-- | Helper for @deserializeTail@
decodeTail :: (Monoid s) => DecodeFrom s s
decodeTail = do
    s <- lift get
    lift $ put mempty
    return s

-- | Hint type
data Hint a = Hint
