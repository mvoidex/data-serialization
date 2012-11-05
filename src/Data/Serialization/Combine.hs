{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Serialization.Combine (
    Combine(..), CombineM(..),
    Encoding(..),
    Decoding(..),

    module Data.Iso
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error

import Data.Iso

infixr 8 .>>
infixr 7 .*.
infixr 6 .+.
infixr 6 .|.
infixl 2 .:.
infixr 1 .?.
infixl 1 .%.

-- | Combining serialize operations
class Combine f where
    (.*.) :: f a -> f b -> f (a, b)
    (.+.) :: f a -> f b -> f (Either a b)
    (.:.) :: f b -> Iso a b -> f a

-- | Extended combining
class Combine f => CombineM f where
    (.|.) :: f a -> f a -> f a
    (.>>) :: f a -> (a -> f b) -> f (a, b)
    pures :: a -> f a
    fails :: String -> f a
    (.?.) :: (a -> Bool) -> f a -> f a
    (.%.) :: f a -> String -> f a

-- | Failable Monad, used just to implement @(.%.)@
class Monad m => MonadFail m where
    failWith :: String -> m a
    failWith = fail
    failMsg :: m a -> String -> m a
    failMsg a _ = a

instance MonadFail Maybe where
    failWith _ = Nothing
instance MonadFail (Either String) where
    failWith msg = Left msg
    failMsg (Left _) msg = Left msg
    failMsg (Right x) _ = Right x
instance (Error e, MonadError e m) => MonadFail m where
    failWith = throwError . strMsg
    failMsg act msg = catchError act $ (\_ -> failWith msg)

-- | Wrapper used to derive @Combine@ and @CombineM@
newtype Encoding m a = Encoding {
    runEncoding :: a -> m () }

instance Monad m => Combine (Encoding m) where
    ~(Encoding l) .*. ~(Encoding r) = Encoding $ \(x, y) -> l x >> r y
    ~(Encoding l) .+. ~(Encoding r) = Encoding $ either l r
    ~(Encoding e) .:. iso = Encoding $ e . morph iso

instance (Alternative m, MonadFail m) => CombineM (Encoding m) where
    ~(Encoding l) .|. ~(Encoding r) = Encoding $ \x -> l x <|> r x
    ~(Encoding s) .>> f = Encoding $ \(x, y) -> s x >> runEncoding (f x) y
    pures v = Encoding $ \x -> return ()
    fails s = Encoding (const $ failWith s)
    p .?. ~(Encoding s) = Encoding $ \x -> if p x then s x else failWith "Predicate doesn't match"
    ~(Encoding s) .%. e = Encoding $ \x -> failMsg (s x) e

-- | Deserializer, used to derive @Combine@ and @CombineM@
newtype Decoding m a = Decoding {
    runDecoding :: m a }

instance (Alternative m) => Combine (Decoding m) where
    ~(Decoding l) .*. ~(Decoding r) = Decoding $ (,) <$> l <*> r
    ~(Decoding l) .+. ~(Decoding r) = Decoding $ (Left <$> l) <|> (Right <$> r)
    ~(Decoding d) .:. iso = Decoding $ fmap (comorph iso) d

instance (Alternative m, MonadFail m) => CombineM (Decoding m) where
    ~(Decoding l) .|. ~(Decoding r) = Decoding $ l <|> r
    ~(Decoding d) .>> f = Decoding $ do
        x <- d
        y <- runDecoding $ f x
        return (x, y)
    pures = Decoding . return
    fails s = Decoding $ failWith s
    p .?. ~(Decoding d) = Decoding $ do
        x <- d
        if p x then return x else failWith "Predicate doesn't match"
    ~(Decoding d) .%. e = Decoding $ failMsg d e
