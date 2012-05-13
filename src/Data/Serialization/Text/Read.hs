{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Read (
    ReadText,
    readable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Serialization.Combine
import Data.Serialization.Deserialize
import Data.Serialization.Serializable

newtype ReadText a = ReadText {
    readText :: ErrorT String (State String) a }
        deriving (Functor, Applicative, Alternative, Monad)

readable :: (Read a) => Deserialize ReadText a
readable = Deserialize r where
    r = ReadText $ do
        s <- lift get
        case reads s of
            [(v, s')] -> lift (put s') >> return v
            _ -> throwError $ "Can't read value"

instance Deserialization ReadText String where
    runDeserialization (ReadText d) s = evalState (runErrorT d) s
    deserializationEof _ = ReadText $ do
        s <- lift get
        when (null s) $ throwError "EOF"
    deserializeTail = ReadText $ do
        s <- lift get
        lift (put "")
        return s
