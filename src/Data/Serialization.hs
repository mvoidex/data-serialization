-- | Serialization
--
-- Let's start by simple example. Suppose you have type:
--
-- >data Test = Test1 Int String | Test2 [Double]
-- >    deriving (Generic)
--
-- There are two ways two make serializer: derive from @Serializable@ or write your own.
-- In most cases it's better to derive, but sometimes you may need to create specific serializer.
--
-- >instance Serializable (Codec String ShowText ReadText) Test
-- >
-- >test :: Codec String ShowText ReadText Test
-- >test = ser
-- >-- encode test (Test1 123 "hello") = Right "123 \"hello\""
-- >-- encode test (Test2 [1,2,3]) = Right "[1.0,2.0,3.0]"
--
-- To define custom serializer use functions from @Combine@ class and @giso@.
--
-- >test2 :: Codec String ShowText ReadText Test
-- >test2 =
-- >    dat ( -- datatype
-- >        ctor ( -- first constructor, @Test1@
-- >            stor ser -- selector of @Int@
-- >            .*.
-- >            stor ser) -- selector of @String@
-- >        .+.
-- >        ctor (stor $ many ser)) -- second constructor (@Test2) with only one selector
-- >    .:.
-- >    giso -- convert generic representation to @Test@
-- >
-- >-- Note the difference between test and test2:
-- >-- encode test2 (Test2 [1,2,3]) = Right "1.0 2.0 3.0"
--
-- Module @Wrap@ provides simple way to create new serializers.
--
-- To create serializer based on existing one, just wrap it with newtype and derive @Combine@.
--
-- >newtype ZeroDelim a = ZeroDelim { runZero :: Codec String ShowText ReadText a }
-- >    deriving (Generic)
-- >
-- >instance Combine ZeroDelim where
-- >    l .*. r = ZeroDelim $ runZero l .* literal ser (0 :: Int) .*. runZero r
-- >    l .+. r = ZeroDelim $ runZero l .+. runZero r
-- >    s .:. iso = ZeroDelim $ runZero s .:. iso
-- >
-- >instance Encoder ZeroDelim String
-- >instance Decoder ZeroDelim String
-- >
-- >test3 :: ZeroDelim Test
-- >test3 = ser
-- >-- Note zero between fields
-- >-- encode test3 (Test1 123 "hello") = Right "123 0 \"hello\""
--
module Data.Serialization (
    module Data.Serialization.Combine,
    module Data.Serialization.Codec,
    module Data.Serialization.Combinators
    ) where

import Data.Serialization.Combine
import Data.Serialization.Codec
import Data.Serialization.Combinators
