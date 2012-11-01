{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Show (
    ShowText(..),
    showable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Writer
import Data.Serialization.Combine
import Data.Serialization.Codec

newtype ShowText a = ShowText {
    showText :: WriterT [String] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad, MonadError String)

showable :: (Show a) => Encoding ShowText a
showable = Encoding w where
    w = ShowText . tell . return . show

instance MetaEncode ShowText
instance Serializer ShowText String where
    serialize (ShowText v) = fmap unwords $ execWriterT v
    serializeTail v = ShowText $ tell [v]
