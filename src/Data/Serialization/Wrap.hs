{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DefaultSignatures, TypeFamilies #-}

-- | Module provides helpers to implement serializer as simple as possible
--
-- 
module Data.Serialization.Wrap (
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

-- | Wrap this with newtype and derive from @MetaEncode@ and @Serializer@
type EncodeTo s a = WriterT s (Either String) a

-- | Wrap this with newtype and derive from @MetaDecode@ and @Deserializer@
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
