{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}

-- | Module provides helpers to implement serializer as simple as possible
--
-- 
module Data.Serialization.Wrap (
    -- * Types to wrap over
    EncodeTo, DecodeFrom,
    -- * Wrap classes
    EncodeWrap(..), DecodeWrap(..),
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

import Data.Serialization.Combine

-- | Wrap this with newtype and derive from @MetaEncode@, @EncodeWrap@ and @Serializer@
type EncodeTo s a = WriterT s (Either String) a

-- | Wrap this with newtype and derive from @MetaDecode@, @DecodeWrap@ and @Deserializer@
type DecodeFrom s a = ErrorT String (State s) a

-- | Derive from it to auto-derive from @Serializer@
class (Alternative c, Monad c, Monoid s) => EncodeWrap c s | c -> s where
    encodeWrap :: EncodeTo s a -> c a
    encodeUnwrap :: c a -> EncodeTo s a

-- | Derive from it to auto-derive from @Deserializer@
class (Alternative c, Monad c, Monoid s, Eq s) => DecodeWrap c s | c -> s where
    decodeWrap :: DecodeFrom s a -> c a
    decodeUnwrap :: c a -> DecodeFrom s a

-- | Helper function to produce some output
encodePart :: (Monoid s, EncodeWrap c s) => (a -> Either String s) -> Encoding c a
encodePart f = Encoding $ encodeWrap . either (lift . Left) tell . f

-- | Helper function to consume some input
decodePart :: (Monoid s, Eq s, DecodeWrap c s) => (s -> Either String (a, s)) -> Decoding c a
decodePart f = Decoding $ decodeWrap $ do
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
