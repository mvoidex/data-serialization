module Data.Serialization.Deserialize (
    Deserialize(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Serialization.Combine

newtype Deserialize m a = Deserialize {
    runDeserialize :: m a }

instance (Applicative m, Alternative m, Monad m) => Combine (Deserialize m) where
    (Deserialize l) .**. (Deserialize r) = Deserialize $ (,) <$> l <*> r
    (Deserialize l) .++. (Deserialize r) = Deserialize $ (Left <$> l) <|> (Right <$> r)
    (Deserialize l) .+. (Deserialize r) = Deserialize $ l <|> r
    (Iso ab ba) <<>> (Deserialize v) = Deserialize $ fmap ab v
    (Deserialize l) .*>> f = Deserialize $ do
        x <- l
        y <- runDeserialize (f x)
        return (x, y)
    pures v = Deserialize (pure v)
    fails = Deserialize empty
