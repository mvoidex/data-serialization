{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts, ConstraintKinds, UndecidableInstances #-}

module Data.Iso (
    Iso(..), coiso,
    -- * Simplified type representation
    Data(..), Ctor(..), Stor(..),
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

-- | Data with name
data Data a = Data String a

-- | Constructor of type with name
data Ctor a = Ctor String a

-- | Selector with name if any
data Stor a = Stor (Maybe String) a

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
    type GenRep (M1 D c a) = Data (GenRep a)
    genTo v@(M1 x) = Data (datatypeName v) (genTo x)
    genFrom (Data _ x) = M1 (genFrom x)

instance (GenIso a, Constructor c) => GenIso (M1 C c a) where
    type GenRep (M1 C c a) = Ctor (GenRep a)
    genTo v@(M1 x) = Ctor (conName v) (genTo x)
    genFrom (Ctor _ x) = M1 (genFrom x)

instance (GenIso a, Selector c) => GenIso (M1 S c a) where
    type GenRep (M1 S c a) = Stor (GenRep a)
    genTo v@(M1 x) = Stor (nm $ selName v) (genTo x) where
        nm "" = Nothing
        nm s = Just s
    genFrom (Stor _ x) = M1 (genFrom x)

instance GenIso (K1 i a) where
    type GenRep (K1 i a) = a
    genTo (K1 x) = x
    genFrom x = K1 x

-- | Representation of type
type IsoRep a = GenRep (Rep a)

-- | Constraint whether type can be automatically derived thgough its representation
type GenIsoDerivable cls a = (Generic a, GenIso (Rep a), cls (IsoRep a))

-- | Generic iso between type and its representation
giso :: (Generic a, GenIso (Rep a)) => Iso a (IsoRep a)
giso = Iso (genTo . from) (to . genFrom)

-- | Newtype wrapped
type family WrappedType a :: *
type instance WrappedType (Data (Ctor (Stor a))) = a

-- | Class for newtype wrappers to wrap/unwrap
class Wrapper a where
    wrap :: WrappedType (IsoRep a) -> a
    unwrap :: a -> WrappedType (IsoRep a)

-- | Default instance
instance (Generic a, GenIso (Rep a), IsoRep a ~ Data (Ctor (Stor x))) => Wrapper a where
    wrap v = comorph giso $ Data "" (Ctor "" (Stor Nothing v))
    unwrap v = (\(Data _ (Ctor _ (Stor _ x))) -> x) $ morph giso v    
