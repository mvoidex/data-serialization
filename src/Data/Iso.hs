{-# LANGUAGE TemplateHaskell #-}

module Data.Iso (
    Iso(..), makeIso
    ) where

import Control.Arrow
import Language.Haskell.TH hiding (clause)

data Iso a b = Iso (a -> b) (b -> a)

printQ :: String -> Q ()
printQ s = runIO $ putStrLn s

-- | Make Iso between data and structure, based on (,) and Either.
-- $(makeIso "foo" ''Foo)
-- expands to
-- foo :: Iso Foo a
makeIso :: String -> Name -> Q [Dec]
makeIso n t = do
    i <- reify t
    case i of
        TyConI dec -> isoData n dec
        _ -> do
            report False "Iso can only be made for data"
            return []

isoData :: String -> Dec -> Q [Dec]
isoData fname (NewtypeD _ name vars con _) = declare fname name vars $ ctors [ctorIso con]
isoData fname (DataD _ name vars cons _) = declare fname name vars $ ctors $ map ctorIso cons

data CtorIso = CtorIso {
    fromCtor :: (Pat, Exp),
    toCtor :: (Pat, Exp),
    domain :: Type }

ctorIso :: Con -> CtorIso
ctorIso (NormalC name ts) = CtorIso from to dom where
    from = (ConP name (map VarP args), foldr1 tup2 (map VarE args))
    to = (foldr1 ptup2 (map VarP args), foldl AppE (ConE name) (map VarE args))
    dom = foldr1 ttup2 ts'
    
    tup2 l r = TupE [l, r]
    ptup2 l r = TupP [l, r]
    ttup2 l r = TupleT 2 `AppT` l `AppT` r
    
    args = map (\i -> mkName ("x" ++ show i)) [1 .. length ts]
    ts' = map snd ts

data DataIso = DataIso {
    fromData :: [(Pat, Exp)],
    toData :: [(Pat, Exp)],
    dataDomain :: Type }

nLeft :: Name
nLeft = mkName "Left"

nRight :: Name
nRight = mkName "Right"

nEither :: Name
nEither = mkName "Either"

nIso :: Name
nIso = mkName "Iso"

ctors :: [CtorIso] -> DataIso
ctors = foldr1 combine . map dataIso where
    dataIso (CtorIso f t d) = DataIso [f] [t] d
    combine (DataIso fl tl dl) (DataIso fr tr dr) = DataIso f t d where
        f = map (second (AppE (ConE nLeft))) fl ++ map (second (AppE (ConE nRight))) fr
        t = map (first (ConP nLeft . return)) tl ++ map (first (ConP nRight . return )) tr
        d = ConT nEither `AppT` dl `AppT` dr

clause :: (Pat, Exp) -> Clause
clause (p, e) = Clause [p] (NormalB e) []

bndrName :: TyVarBndr -> Name
bndrName (PlainTV n) = n
bndrName (KindedTV n _) = n

declare :: String -> Name -> [TyVarBndr] -> DataIso -> Q [Dec]
declare fname name vars (DataIso from to dom) = return [decl, def] where
    decl = SigD fName (ForallT vars [] $ (ConT nIso `AppT` thisT `AppT` dom))
    def = FunD fName [Clause [] (NormalB (ConE nIso `AppE` (VarE f) `AppE` (VarE cof))) wheres] where
        f = mkName "morphism"
        cof = mkName "comorphism"
        wheres = [morph, comorph]
        morph = FunD f (map clause from)
        comorph = FunD cof (map clause to)
    thisT = foldl AppT (ConT name) (map (VarT . bndrName) vars)
    fName = mkName fname
