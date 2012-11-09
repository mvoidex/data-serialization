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
    recode
    ) where

import Control.Applicative
import Data.Monoid
import Data.Serialization.Combine
import Data.Serialization.Generic
import Data.Serialization.Wrap

-- | Encoder and decoder
data Codec s sm dm a = Codec {
    encoder :: Encoding sm a,
    decoder :: Decoding dm a }

type CodecT s sm dm a = Codec s (sm s) (dm s) a

instance (Combine (Encoding sm), Combine (Decoding dm)) => Combine (Codec s sm dm) where
    ~(Codec el dl) .*. ~(Codec er dr) = Codec (el .*. er) (dl .*. dr)
    ~(Codec el dl) .+. ~(Codec er dr) = Codec (el .+. er) (dl .+. dr)
    ~(Codec e d) .:. iso = Codec (e .:. iso) (d .:. iso)

instance (GenericCombine (Encoding sm), GenericCombine (Decoding dm)) => GenericCombine (Codec s sm dm) where
    genericData s ~(Codec e d) = Codec (genericData s e) (genericData s d)
    genericCtor s ~(Codec e d) = Codec (genericCtor s e) (genericCtor s d)
    genericStor s ~(Codec e d) = Codec (genericStor s e) (genericStor s d)

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
eof :: (Serializer s sm, Deserializer s dm) => Codec s sm dm ()
eof = eof' where
    h :: Codec s sm dm () -> Hint s
    h _ = Hint
    eof' = codec
        (Encoding $ const (return ()))
        (Decoding (deserializeEof (h eof')))

-- | Match anything: produce as is and consumes all
anything :: (Serializer s sm, Deserializer s dm) => Codec s sm dm s
anything = codec (Encoding serializeTail) (Decoding deserializeTail)

-- | Encoder
class Encoder s c | c -> s where
    encode :: c a -> a -> Either String s

-- | Decoder
class Decoder s c | c -> s where
    decode :: c a -> s -> Either String a

instance (Serializer s sm) => Encoder s (Encoding sm) where
    encode ~(Encoding s) = serialize . s

instance (Deserializer s dm) => Decoder s (Decoding dm) where
    decode ~(Decoding d) = deserialize d

instance (Serializer s sm) => Encoder s (Codec s sm dm) where
    encode ~(Codec e _) = encode e

instance (Deserializer s dm) => Decoder s (Codec s sm dm) where
    decode ~(Codec _ e) = decode e

-- | Make convertible by @encode@ and @decode@
recode :: (Serializer s sm, Deserializer s dm) => Codec s sm dm a -> Convertible a s
recode s = Convertible (encode s) (decode s)
