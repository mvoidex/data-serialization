module Data.Serialization.Serialize (
    Serialize(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Serialization.Combine

newtype Serialize m a = Serialize {
    runSerialize :: a -> m () }

instance (Monad m, Applicative m, Alternative m) => Combine (Serialize m) where
    (Serialize l) .**. (Serialize r) = Serialize $ \(x, y) -> l x >> r y
    (Serialize l) .++. (Serialize r) = Serialize f where
        f (Left x) = l x
        f (Right y) = r y
    (Serialize l) .+. (Serialize r) = Serialize $ \x -> l x <|> r x
    (Iso ab ba) <<>> (Serialize v) = Serialize $ v . ba
    (Serialize l) .*>> f = Serialize $ \(x, y) -> l x >> runSerialize (f x) y
    pures v = Serialize $ \x -> return ()
    fails = Serialize (const empty)
