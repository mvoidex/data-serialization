{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, FunctionalDependencies, DefaultSignatures, TypeFamilies, ConstraintKinds, OverlappingInstances #-}

module Data.Serialization.Codec (
    -- * Serializer and deserializer together
    Codec(..), CodecT,
    Serializer(..), Deserializer(..),
    Hint(..),
    codec,
    Convertible(..), coconvertble,
    eof, anything,
    -- * Encoding and decoding
    Encoder(..), Decoder(..),
    recode,
    -- * Serializable
    Serializable(..), GenSerializable(..)
    ) where

import Control.Applicative
import Data.Monoid
import Data.Serialization.Combine
import Data.Serialization.Wrap

-- | Encoder and decoder
data Codec s sm dm a = Codec {
    encoder :: Encoding sm a,
    decoder :: Decoding dm a }

type CodecT s sm dm a = Codec s (sm s) (dm s) a

instance (Combine (Encoding sm), Combine (Decoding dm)) => Combine (Codec s sm dm) where
    ~(Codec sl dl) .*. ~(Codec sr dr) = Codec (sl .*. sr) (dl .*. dr)
    ~(Codec sl dl) .+. ~(Codec sr dr) = Codec (sl .+. sr) (dl .+. dr)
    ~(Codec s d) .:. iso = Codec (s .:. iso) (d .:. iso)
    dat ~(Codec s d) = Codec (dat s) (dat d)
    ctor ~(Codec s d) = Codec (ctor s) (ctor d)
    stor ~(Codec s d) = Codec (stor s) (stor d)

instance (CombineM (Encoding sm), CombineM (Decoding dm)) => CombineM (Codec s sm dm) where
    ~(Codec sl dl) .|. ~(Codec sr dr) = Codec (sl .|. sr) (dl .|. dr)
    ~(Codec s d) .>> f = Codec (s .>> (encoder . f)) (d .>> (decoder . f))
    pures v = Codec (pures v) (pures v)
    fails s = Codec (fails s) (fails s)
    p .?. ~(Codec s d) = Codec (p .?. s) (p .?. d)
    ~(Codec s d) .%. e = Codec (s .%. e) (d .%. e)

data Convertible a b = Convertible {
    convertTo :: a -> Either String b,
    convertFrom :: b -> Either String a }

coconvertble :: Convertible a b -> Convertible b a
coconvertble (Convertible t f) = Convertible f t

codec :: Encoding sm a -> Decoding dm a -> Codec s sm dm a
codec  = Codec

-- | Match eof
eof :: (Serializer sm s, Deserializer dm s) => Codec s sm dm ()
eof = eof' where
    h :: Codec s sm dm () -> Hint s
    h _ = Hint
    eof' = codec
        (Encoding $ const (return ()))
        (Decoding (deserializeEof (h eof')))

-- | Match anything: produce as is and consumes all
anything :: (Serializer sm s, Deserializer dm s) => Codec s sm dm s
anything = codec (Encoding serializeTail) (Decoding deserializeTail)

-- | Encoder
class Encoder c s | c -> s where
    encode :: c a -> a -> Either String s
    default encode :: (Wrapper t, t ~ c a, WrappedType (IsoRep t) ~ k a, Encoder k s) => c a -> a -> Either String s
    encode = encode . unwrap

-- | Decoder
class Decoder c s | c -> s where
    decode :: c a -> s -> Either String a
    default decode :: (Wrapper t, t ~ c a, WrappedType (IsoRep t) ~ k a, Decoder k s) => c a -> s -> Either String a
    decode = decode . unwrap

instance (Serializer sm s) => Encoder (Encoding sm) s where
    encode ~(Encoding s) = serialize . s

instance (Deserializer dm s) => Decoder (Decoding dm) s where
    decode ~(Decoding d) = deserialize d

instance (Serializer sm s) => Encoder (Codec s sm dm) s where
    encode ~(Codec e _) = encode e

instance (Deserializer dm s) => Decoder (Codec s sm dm) s where
    decode ~(Codec _ e) = decode e

-- | Make convertible by @encode@ and @decode@
recode :: (Serializer sm s, Deserializer dm s) => Codec s sm dm a -> Convertible a s
recode s = Convertible (encode s) (decode s)

-- | Serializable value
class (Combine f) => Serializable f a where
    ser :: f a
    default ser :: (GenIsoDerivable (GenSerializable f) a) => f a
    ser = gser .:. giso

-- | Generic serializable
class (Combine f) => GenSerializable f a where
    gser :: f a

instance Serializable f a => GenSerializable f a where
    gser = ser

instance (GenSerializable f a, GenSerializable f b) => GenSerializable f (a, b) where
    gser = gser .*. gser

instance (GenSerializable f a, GenSerializable f b) => GenSerializable f (Either a b) where
    gser = gser .+. gser

instance (GenSerializable f a) => GenSerializable f (Data a) where
    gser = gser .:. Iso (\(Data _ x) -> x) (Data "")

instance (GenSerializable f a) => GenSerializable f (Ctor a) where
    gser = gser .:. Iso (\(Ctor _ x) -> x) (Ctor "")

instance (GenSerializable f a) => GenSerializable f (Stor a) where
    gser = gser .:. Iso (\(Stor _ x) -> x) (Stor Nothing)
