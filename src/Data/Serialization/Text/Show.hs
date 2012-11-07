{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Show (
    ShowText(..),
    showable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Writer
import Data.Serialization.Combine
import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec
import GHC.Generics

newtype ShowText a = ShowText { showText :: EncodeTo [String] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadError String, MonadWriter [String], Generic)

instance GenericEncode ShowText
instance Serializer ShowText String where
    serialize (ShowText v) = fmap unwords $ execWriterT v
    serializeTail v = ShowText $ tell [v]

showable :: (Show a) => Encoding ShowText a
showable = encodePart $ return . return . show
