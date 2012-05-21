{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Show (
    ShowTextT(..), ShowText,
    showable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Serialization.Combine
import Data.Serialization.Serialize
import Data.Serialization.Serializable

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text as T

class StringLike s where
    fromString :: String -> s
    toString :: s -> String
    concats :: [s] -> s
    fromWords :: [s] -> s

instance StringLike String where
    fromString = id
    toString = id
    concats = concat
    fromWords = unwords

instance StringLike B.ByteString where
    fromString = B.pack
    toString = B.unpack
    concats = B.concat
    fromWords = B.unwords

instance StringLike L.ByteString where
    fromString = L.pack
    toString = L.unpack
    concats = L.concat
    fromWords = L.unwords

instance StringLike Text where
    fromString = T.pack
    toString = T.unpack
    concats = T.concat
    fromWords = T.unwords

newtype ShowTextT s a = ShowTextT {
    showText :: WriterT [s] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

type ShowText a = ShowTextT String a

showable :: (Show a, StringLike s) => Serialize (ShowTextT s) a
showable = Serialize w where
    w = ShowTextT . tell . return . fromString . show

instance (StringLike s) => Serialization (ShowTextT s) s where
    runSerialization (ShowTextT v) = fmap fromWords $ execWriterT v
    serializeTail v = ShowTextT $ tell [v]
