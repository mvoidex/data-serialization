{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}

module Data.Serialization.Codec (
    Codec(..), CodecT,
    Serializer(..), Deserializer(..),
    Hint(..),
    codec,
    Convertible(..), coconvertble,
    eof, anything,
    Encoder(..), Decoder(..),
    recode
    ) where

import Control.Applicative
import Data.Serialization.Combine

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

class (MetaEncode sm, Monad sm, Applicative sm, Alternative sm) => Serializer sm s where
    serialize :: sm () -> Either String s
    serializeTail :: s -> sm ()

data Hint a = Hint

class (MetaDecode dm, Monad dm, Applicative dm, Alternative dm) => Deserializer dm s where
    deserialize :: dm a -> s -> Either String a
    deserializeEof :: Hint s -> dm ()
    deserializeTail :: dm s

data Convertible a b = Convertible {
    convertTo :: a -> Either String b,
    convertFrom :: b -> Either String a }

coconvertble :: Convertible a b -> Convertible b a
coconvertble (Convertible t f) = Convertible f t

codec :: Encoding sm a -> Decoding dm a -> Codec s sm dm a
codec  = Codec

eof :: (Serializer sm s, Deserializer dm s) => Codec s sm dm ()
eof = eof' where
    h :: Codec s sm dm () -> Hint s
    h _ = Hint
    eof' = codec
        (Encoding $ const (return ()))
        (Decoding (deserializeEof (h eof')))

anything :: (Serializer sm s, Deserializer dm s) => Codec s sm dm s
anything = codec (Encoding serializeTail) (Decoding deserializeTail)

class Encoder c s | c -> s where
    encode :: c a -> a -> Either String s

class Decoder c s | c -> s where
    decode :: c a -> s -> Either String a

instance (Serializer sm s) => Encoder (Encoding sm) s where
    encode ~(Encoding s) = serialize . s

instance (Deserializer dm s) => Decoder (Decoding dm) s where
    decode ~(Decoding d) = deserialize d

instance (Serializer sm s) => Encoder (Codec s sm dm) s where
    encode ~(Codec e _) = encode e

instance (Deserializer dm s) => Decoder (Codec s sm dm) s where
    decode ~(Codec _ e) = decode e

recode :: (Serializer sm s, Deserializer dm s) => Codec s sm dm a -> Convertible a s
recode s = Convertible (encode s) (decode s)
