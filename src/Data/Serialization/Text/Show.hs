{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Show (
    ShowText,
    showable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Serialization.Combine
import Data.Serialization.Serialize
import Data.Serialization.Serializable

newtype ShowText a = ShowText {
    showText :: WriterT [String] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

showable :: (Show a) => Serialize ShowText a
showable = Serialize w where
    w = ShowText . tell . return . show

instance Serialization ShowText String where
    runSerialization (ShowText v) = fmap unwords $ execWriterT v
    serializeTail v = ShowText $ tell [v]
