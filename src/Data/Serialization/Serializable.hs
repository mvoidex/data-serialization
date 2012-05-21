{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Serialization.Serializable (
    Serializable(..),
    Serialization(..), Deserialization(..),
    serializable,
    eof, anything,
    encode, decode
    ) where

import Control.Applicative
import Control.Monad
import Data.Serialization.Combine
import Data.Serialization.Serialize
import Data.Serialization.Deserialize

data Serializable s sm dm a = Serializable {
    serializer :: Serialize sm a,
    deserializer :: Deserialize dm a }

instance (Monad sm, Applicative sm, Alternative sm, Monad dm, Applicative dm, Alternative dm) => Combine (Serializable s sm dm) where
    ~(Serializable sl dl) .**. ~(Serializable sr dr) = Serializable (sl .**. sr) (dl .**. dr)
    ~(Serializable sl dl) .++. ~(Serializable sr dr) = Serializable (sl .++. sr) (dl .++. dr)
    ~(Serializable sl dl) .+. ~(Serializable sr dr) = Serializable (sl .+. sr) (dl .+. dr)
    f <<>> ~(Serializable s d) = Serializable (f <<>> s) (f <<>> d)
    ~(Serializable s d) .:. f = Serializable (s .:. f) (d .:. f)
    ~(Serializable s d) .*>> f = Serializable (s .*>> (serializer . f)) (d .*>> (deserializer . f))
    pures v = Serializable (pures v) (pures v)
    fails = Serializable fails fails

class (Monad sm, Applicative sm, Alternative sm) => Serialization sm s where
    runSerialization :: sm () -> Either String s
    serializeTail :: s -> sm ()

data Hint a = Hint

class (Monad dm, Applicative dm, Alternative dm) => Deserialization dm s where
    runDeserialization :: dm a -> s -> Either String a
    deserializationEof :: Hint s -> dm ()
    deserializeTail :: dm s

serializable
    :: (Serialization sm s, Deserialization dm s)
    => Serialize sm a
    -> Deserialize dm a
    -> Serializable s sm dm a
serializable p g = Serializable p g

eof :: (Serialization sm s, Deserialization dm s) => Serializable s sm dm ()
eof = eof' where
    h :: Serializable s sm dm () -> Hint s
    h _ = Hint
    eof' = serializable
        (Serialize $ const (return ()))
        (Deserialize (deserializationEof (h eof')))

anything :: (Serialization sm s, Deserialization dm s) => Serializable s sm dm s
anything = serializable (Serialize serializeTail) (Deserialize deserializeTail)

encode :: (Serialization sm s) => Serializable s sm dm a -> a -> Either String s
encode ~(Serializable ~(Serialize s) _) v = runSerialization $ s v

decode :: (Deserialization dm s) => Serializable s sm dm a -> s -> Either String a
decode ~(Serializable _ ~(Deserialize d)) s = runDeserialization d s
