{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverlappingInstances #-}

module Data.Serialization.Text.Read (
    ReadText(..),
    readable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Serialization.Combine
import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec
import GHC.Generics

newtype ReadText a = ReadText { readText :: DecodeFrom String a }
    deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadState String, MonadError String, Generic)

instance GenericDecode ReadText
instance Deserializer ReadText String

readable :: (Read a) => Decoding ReadText a
readable = decodePart f where
    f s = case reads s of
        [(v, s')] -> Right (v, s')
        _ -> Left "Can't read value"
