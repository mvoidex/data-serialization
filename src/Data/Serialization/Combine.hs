{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Serialization.Combine (
    Combine(..), CombineM(..),
    MetaEncode(..), Encoding(..),
    MetaDecode(..), Decoding(..),

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
    dat :: f a -> f (Data a)
    ctor :: f a -> f (Ctor a)
    stor :: f a -> f (Stor a)

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

-- | Encoder for constructor and selectors, used to provides it's named to serializer
-- by default just ignores names
class MetaEncode m where
    encodeData :: (a -> m ()) -> Data a -> m ()
    encodeCtor :: (a -> m ()) -> Ctor a -> m ()
    encodeStor :: (a -> m ()) -> Stor a -> m ()

    encodeData s (Data _ x) = s x
    encodeCtor s (Ctor _ x) = s x
    encodeStor s (Stor _ x) = s x

-- | Wrapper used to derive @Combine@ and @CombineM@
newtype Encoding m a = Encoding {
    runEncoding :: a -> m () }

instance (MetaEncode m, Monad m) => Combine (Encoding m) where
    ~(Encoding l) .*. ~(Encoding r) = Encoding $ \(x, y) -> l x >> r y
    ~(Encoding l) .+. ~(Encoding r) = Encoding $ either l r
    ~(Encoding s) .:. iso = Encoding $ s . morph iso
    dat ~(Encoding s) = Encoding $ encodeData s
    ctor ~(Encoding s) = Encoding $ encodeCtor s
    stor ~(Encoding s) = Encoding $ encodeStor s

instance (MetaEncode m, Alternative m, MonadFail m) => CombineM (Encoding m) where
    ~(Encoding l) .|. ~(Encoding r) = Encoding $ \x -> l x <|> r x
    ~(Encoding s) .>> f = Encoding $ \(x, y) -> s x >> runEncoding (f x) y
    pures v = Encoding $ \x -> return ()
    fails s = Encoding (const $ failWith s)
    p .?. ~(Encoding s) = Encoding $ \x -> if p x then s x else failWith "Predicate doesn't match"
    ~(Encoding s) .%. e = Encoding $ \x -> failMsg (s x) e

-- | Decoder for constructors and selectors, provides it's names to deserializer
-- By default sets names to empty strings
class (Functor m) => MetaDecode m where
    decodeData :: m a -> m (Data a)
    decodeCtor :: m a -> m (Ctor a)
    decodeStor :: m a -> m (Stor a)

    decodeData = fmap (Data "")
    decodeCtor = fmap (Ctor "")
    decodeStor = fmap (Stor Nothing)

-- | Deserializer, used to derive @Combine@ and @CombineM@
newtype Decoding m a = Decoding {
    runDecoding :: m a }

instance (MetaDecode m, Alternative m) => Combine (Decoding m) where
    ~(Decoding l) .*. ~(Decoding r) = Decoding $ (,) <$> l <*> r
    ~(Decoding l) .+. ~(Decoding r) = Decoding $ (Left <$> l) <|> (Right <$> r)
    ~(Decoding d) .:. iso = Decoding $ fmap (comorph iso) d
    dat ~(Decoding d) = Decoding $ decodeData d
    ctor ~(Decoding d) = Decoding $ decodeCtor d
    stor ~(Decoding d) = Decoding $ decodeStor d

instance (MetaDecode m, Alternative m, MonadFail m) => CombineM (Decoding m) where
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
