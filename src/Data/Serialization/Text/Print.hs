{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Serialization.Text.Print (
    Print(..),
    printWith, printStr, printShow
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.String
import Data.Serialization.Combine
import Data.Serialization.Codec

newtype Print s a = Print {
    runPrint :: WriterT [s] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

printWith :: (a -> s) -> Encoding (Print s) a
printWith f = Encoding w where
    w = Print . tell . return . f

printStr :: (IsString s) => (a -> String) -> Encoding (Print s) a
printStr f = Encoding w where
    w = Print . tell . return . fromString . f

printShow :: (IsString s, Show a) => Encoding (Print s) a
printShow = Encoding w where
    w = Print . tell . return . fromString . show

instance MetaEncode (Print s)
instance (Monoid s) => Serializer (Print s) s where
    serialize (Print v) = fmap mconcat $ execWriterT v
    serializeTail v = Print $ tell [v]
