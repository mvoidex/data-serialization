{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Read (
    ReadText(..),
    readable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Serialization.Combine
import Data.Serialization.Codec

newtype ReadText a = ReadText {
    readText :: ErrorT String (State String) a }
        deriving (Functor, Applicative, Alternative, Monad, MonadError String)

readable :: (Read a) => Decoding ReadText a
readable = Decoding r where
    r = ReadText $ do
        s <- lift get
        case reads s of
            [(v, s')] -> lift (put s') >> return v
            _ -> throwError $ "Can't read value"

instance MetaDecode ReadText
instance Deserializer ReadText String where
    deserialize (ReadText d) s = evalState (runErrorT d) s
    deserializeEof _ = ReadText $ do
        s <- lift get
        when (not $ null s) $ throwError "EOF expected"
    deserializeTail = ReadText $ do
        s <- lift get
        lift (put "")
        return s
