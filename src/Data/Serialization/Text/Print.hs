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
import Data.Serialization.Serialize
import Data.Serialization.Serializable

newtype Print s a = Print {
    runPrint :: WriterT [s] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

printWith :: (a -> s) -> Serialize (Print s) a
printWith f = Serialize w where
    w = Print . tell . return . f

printStr :: (IsString s) => (a -> String) -> Serialize (Print s) a
printStr f = Serialize w where
    w = Print . tell . return . fromString . f

printShow :: (IsString s, Show a) => Serialize (Print s) a
printShow = Serialize w where
    w = Print . tell . return . fromString . show

instance (Monoid s) => Serialization (Print s) s where
    runSerialization (Print v) = fmap mconcat $ execWriterT v
    serializeTail v = Print $ tell [v]
