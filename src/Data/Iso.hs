{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts, ConstraintKinds, UndecidableInstances #-}

module Data.Iso (
    Iso(..), coiso,
    -- * Simplified type representation
    Data(..), Ctor(..), Stor(..),
    dataName, ctorName, storName,
    GenIso(..),
    IsoRep, GenIsoDerivable,
    giso,
    -- * Generalized newtype wrapping
    WrappedType, Wrapper(..)
    ) where

import GHC.Generics

data Iso a b = Iso {
    morph :: a -> b,
    comorph :: b -> a }

coiso :: Iso a b -> Iso b a
coiso (Iso m c) = Iso c m

-- | Simplified generic representation for data
data Data c a = Data { unData :: a } deriving (Show)

-- | Simplified generic representation for constructor
data Ctor c a = Ctor { unCtor :: a } deriving (Show)

-- | Simplified generic represnetation for selector
data Stor c a = Stor { unStor :: a } deriving (Show)

-- | Get name of data
dataName :: Datatype c => Data c a -> String
dataName = datatypeName . r where
    r :: Data c a -> M1 D c b p
    r _ = undefined

ctorName :: Constructor c => Ctor c a -> String
ctorName = conName . r where
    r :: Ctor c a -> M1 C c b p
    r _ = undefined

storName :: Selector c => Stor c a -> String
storName = selName . r where
    r :: Stor c a -> M1 S c b p
    r _ = undefined

-- | Isomorphism between generic type representation and representation through products and coproducts
class GenIso g where
    type GenRep g
    genTo :: g a -> GenRep g
    genFrom :: GenRep g -> g a

instance GenIso U1 where
    type GenRep U1 = ()
    genTo U1 = ()
    genFrom () = U1

instance (GenIso a, GenIso b) => GenIso (a :*: b) where
    type GenRep (a :*: b) = (GenRep a, GenRep b)
    genTo (x :*: y) = (genTo x, genTo y)
    genFrom (x, y) = (genFrom x :*: genFrom y)

instance (GenIso a, GenIso b) => GenIso (a :+: b) where
    type GenRep (a :+: b) = Either (GenRep a) (GenRep b)
    genTo (L1 x) = Left (genTo x)
    genTo (R1 y) = Right (genTo y)
    genFrom (Left x) = L1 (genFrom x)
    genFrom (Right y) = R1 (genFrom y)

instance (GenIso a, Datatype c) => GenIso (M1 D c a) where
    type GenRep (M1 D c a) = Data c (GenRep a)
    genTo = Data . genTo . unM1
    genFrom = M1 . genFrom . unData

instance (GenIso a, Constructor c) => GenIso (M1 C c a) where
    type GenRep (M1 C c a) = Ctor c (GenRep a)
    genTo = Ctor . genTo . unM1
    genFrom = M1 . genFrom . unCtor

instance (GenIso a, Selector c) => GenIso (M1 S c a) where
    type GenRep (M1 S c a) = Stor c (GenRep a)
    genTo = Stor . genTo . unM1
    genFrom = M1 . genFrom . unStor

instance GenIso (K1 i a) where
    type GenRep (K1 i a) = a
    genTo = unK1
    genFrom = K1

-- | Representation of type
type IsoRep a = GenRep (Rep a)

-- | Constraint whether type can be automatically derived thgough its representation
type GenIsoDerivable cls a = (Generic a, GenIso (Rep a), cls (IsoRep a))

-- | Generic iso between type and its representation
giso :: (Generic a, GenIso (Rep a)) => Iso a (IsoRep a)
giso = Iso (genTo . from) (to . genFrom)

-- | Newtype wrapped
type family WrappedType a :: *
type instance WrappedType (Data d (Ctor c (Stor s a))) = a

-- | Class for newtype wrappers to wrap/unwrap
class Wrapper a where
    wrap :: WrappedType (IsoRep a) -> a
    unwrap :: a -> WrappedType (IsoRep a)

-- | Default instance
instance (Generic a, GenIso (Rep a), IsoRep a ~ (Data d (Ctor c (Stor s x)))) => Wrapper a where
    wrap v = comorph giso $ Data $ Ctor $ Stor v
    unwrap v = unStor $ unCtor $ unData $ morph giso v    
