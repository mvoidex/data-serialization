data-serialization
==================

Haskell serialization library

Serializing data
------------

Let's start by simple example. Suppose you have type:

<pre>
data Test = Test1 Int String | Test2 [Double]
    deriving (Generic)
</pre>

There are two ways two make serializer: derive from <code>Serializable</code> or write your own.
In most cases it's better to derive, but sometimes you may need to create specific serializer.

<pre>
instance Serializable (Codec String ShowText ReadText) Test

test :: Codec String ShowText ReadText Test
test = ser
-- encode test (Test1 123 "hello") = Right "123 \"hello\""
-- encode test (Test2 [1,2,3]) = Right "[1.0,2.0,3.0]"
</pre>

To define custom serializer use functions from <code>Combine</code> class and <code>giso</code>.

<pre>
test2 :: Codec String ShowText ReadText Test
test2 =
    dat ( -- datatype
        ctor ( -- first constructor, @Test1@
            stor ser -- selector of @Int@
            .*.
            stor ser) -- selector of @String@
        .+.
        ctor (stor $ many ser)) -- second constructor (Test2) with only one selector
    .:.
    giso -- convert generic representation to Test

-- Note the difference between test and test2:
-- encode test2 (Test2 [1,2,3]) = Right "1.0 2.0 3.0"
</pre>

Creating new serializer
------------------

Suppose you want to write serializer to/from list of strings. Wrap <code>EncodeTo [String] a</code> and <code>DecodeFrom [String] a</code> with new types and make empty intances (all you need is derived automatically)

<pre>
newtype ToCSV a = ToCSV { toCSV :: EncodeTo [String] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Generic)

instance MetaEncode ToCSV
instance Serializer ToCSV [String]

newtype FromCSV a = FromCSV { fromCSV :: DecodeFrom [String] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Generic)

-- Implement functions to collect metadata, default implementation just throws it away
instance MetaDecode FromCSV
instance Deserializer FromCSV [String]
</pre>

And that's all. Now write primitive serializers and use.

<pre>
showcsv :: (Show a) =&gt; Encoding ToCSV a
showcscv = encodePart $ return . return . show

readcsv :: (Read a) =&gt; Decoding FromCSV a
readcsv = decodePart f where
    f [] = Left \"EOF\"
    f (c:cs) = case reads c of
        [(v, \"\")] -&gt; Right (v, cs)
        _ -&gt; Left \"Can't\ read value"
</pre>

Wrapped serializer
---------------

To create serializer based on existing one, just wrap it with newtype and derive <code>Combine</code>.

<pre>
newtype ZeroDelim a = ZeroDelim { runZero :: Codec String ShowText ReadText a }
    deriving (Generic)

instance Combine ZeroDelim where
    l .*. r = ZeroDelim $ runZero l .* literal ser (0 :: Int) .*. runZero r
    l .+. r = ZeroDelim $ runZero l .+. runZero r
    s .:. iso = ZeroDelim $ runZero s .:. iso

instance Encoder ZeroDelim String
instance Decoder ZeroDelim String

test3 :: ZeroDelim Test
test3 = ser
-- Note zero between fields
-- encode test3 (Test1 123 "hello") = Right "123 0 \"hello\""
</pre>
