{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Print (
    Print(..),
    printWith, printStr, printShow
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
import Data.String
import Data.Serialization.Combine
import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec
import GHC.Generics

newtype Print s a = Print { runPrint :: EncodeTo s a }
    deriving (Functor, Applicative, Alternative, Monad, MonadError String, MonadWriter s, Generic)

instance GenericEncode (Print s)
instance (Monoid s) => Serializer s (Print s) where
    serialize = encodeTo . runPrint

printWith :: (Monoid s) => (a -> s) -> Encoding (Print s) a
printWith f = encodePart $ return . f

printStr :: (Monoid s, IsString s) => (a -> String) -> Encoding (Print s) a
printStr f = encodePart $ return . fromString . f

printShow :: (Monoid s, IsString s, Show a) => Encoding (Print s) a
printShow = encodePart $ return . fromString . show
