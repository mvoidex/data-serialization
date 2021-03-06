{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DefaultSignatures, TypeFamilies #-}

-- | Module provides helpers to implement serializer as simple as possible
--
-- Suppose you want to write serializer to/from list of strings. Wrap @EncodeTo [String] a@ and @DecodeFrom [String] a@ with new types and make empty intances (all you need is derived automatically)
--
-- >newtype ToCSV a = ToCSV { toCSV :: EncodeTo [String] a }
-- >    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Generic)
-- >
-- >instance GenericEncode ToCSV
-- >instance Serializer [String] ToCSV
-- >
-- >newtype FromCSV a = FromCSV { fromCSV :: DecodeFrom [String] a }
-- >    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Generic)
-- >
-- >-- Implement functions to collect metadata, default implementation just throws it away
-- >instance GenericDecode FromCSV
-- >instance Deserializer [String] FromCSV
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
class (Monad sm, Applicative sm, Alternative sm) => Serializer s sm where
    serialize :: sm () -> Either String s
    serializeTail :: s -> sm ()

    default serializeTail :: (MonadWriter s sm) => s -> sm ()
    serializeTail = tell

-- | Derive to support deserialization for @Decoding@
class (Monad dm, Applicative dm, Alternative dm) => Deserializer s dm where
    deserialize :: dm a -> s -> Either String a
    deserializeEof :: Hint s -> dm ()
    deserializeTail :: dm s

    default deserializeEof :: (Monoid s, Eq s, MonadState s dm, MonadFail dm) => Hint s -> dm ()
    deserializeEof _ = do
        st <- get
        when (st /= mempty) $ failWith "EOF expected"
    default deserializeTail :: (Monoid s, MonadState s dm) => dm s
    deserializeTail = do
        obj <- get
        put mempty
        return obj

-- | Wrap this with newtype and derive from @GenericEncode@ and @Serializer@
type EncodeTo s a = WriterT s (Either String) a

-- | Wrap this with newtype and derive from @GenericDecode@ and @Deserializer@
type DecodeFrom s a = ErrorT String (State s) a

-- | Helper function to produce some output
encodePart :: (MonadWriter s c, MonadError String c) => (a -> Either String s) -> Encoding c a
encodePart f = Encoding $ either throwError tell . f

-- | Helper function to consume some input
decodePart :: (Monoid s, Eq s, MonadState s c, MonadError String c) => (s -> Either String (a, s)) -> Decoding c a
decodePart f = Decoding $ do
    s <- get
    if s == mempty
        then throwError "EOF"
        else do
            (v, s') <- either throwError return $ f s
            put s'
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
