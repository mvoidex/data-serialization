data-serialization
==================

Haskell serialization library


Use combinators to create serializers for products and coproducts and convert to any data with (.:.).

For example:

<pre>
data Result s a =
    ValidResult s a |
    InvalidResult String (Maybe a) |
    UnknownResult [String]
        deriving (Eq, Ord, Read, Show)

type Textual a = Serializable String (ShowTextT String) ReadText a

$(makeIso "result" ''Result)
-- ^ result :: Iso (Result s a) (Either (s, a) (Either (String, Maybe a) [String]))

test :: (Read s, Show s, Read a, Show a) => Textual (Result s a)
test = (
    (textual .**. textual) .++.        -- ^ (s, a) for ValidResult
    (textual .**. try textual) .++.   -- ^ (String, Maybe a) for InvalidResult
    (many textual)                     -- ^ [String] for UknownResult
    .:.
    result                             -- ^ Convert to Result s a
</pre>