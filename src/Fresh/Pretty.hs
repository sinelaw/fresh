{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fresh.Pretty
       ( Pretty(..) )
       where

-- import Data.STRef (STRef)
import Text.PrettyPrint.ANSI.Leijen
import Fresh.Type
import Fresh.Kind (Kind(..))




numToLetter :: Int -> Doc
numToLetter idx =
    if idx < length ['a'..'z']
    then char $ ['a'..'z'] !! idx
    else "t" <> int idx

instance Pretty TCon where
    pretty (TCon (Id s) k) = pretty s

instance Pretty Kind where
    pretty (KArrow k1 k2) = pretty k1 <+> "->" <+> pretty k2
    pretty Star = "*"
    pretty Composite = "@"

instance Pretty Level where
    pretty _ = empty
    -- pretty (LevelAny) = "^^"
    -- pretty (Level x) = "^" <> pretty x

instance Pretty GenVar where
    pretty (GenVar idx k l) = pk name <> pretty l
        where name = numToLetter idx
              pk = if k == Star
                   then id
                   else \x -> x <+> "::" <+> pretty k

instance Pretty CompositeLabelName where
    pretty (CompositeLabelName x) = dquotes $ pretty x

instance Pretty t => Pretty (Composite t) where
    pretty (CompositeLabel name t c) =
        pretty name <> ":" <+> pretty t <> rest
        where rest = case c of
                  CompositeLabel{} -> comma <+> pretty c
                  _ -> pretty c
        -- TODO trailing comma
    pretty (CompositeTerminal) = empty
    pretty (CompositeRemainder t) = " |" <+> pretty t

instance (HasKind t, Pretty t) => Pretty (TypeAST t) where
    pretty (TyAp fun arg) =
        case kind fun of
             Just (KArrow _ KArrow{}) -> pretty arg <+> pretty fun
                 -- TODO red for bad kind app
             _ -> parens $ pretty fun <+> pretty arg
    pretty (TyCon con) = pretty con
    pretty (TyGenVar genVar) = pretty genVar
    pretty (TyGen genVar t) = "forall" <+> pretty genVar <> "." <+> pretty t
    -- TODO
    pretty (TyComp c) = "{" <+> pretty c <+> "}"

instance Pretty EVarName where
    pretty (EVarName s) = text s

instance Pretty Lit where
    pretty (LitNum x) = pretty x
    pretty (LitString x) = pretty x
    pretty (LitBool x) = pretty x

instance Pretty (Expr a) where
    pretty (ELit a l) = pretty l
    pretty (EVar a varName) = pretty varName
    pretty (ELam a varName e) = parens $ "\\" <> pretty varName <+> "->" <+> pretty e
    pretty (EALam a varName t e) = parens $ "\\" <> parens (pretty varName <+> "::" <+> pretty t) <+> "->" <+> pretty e
    pretty (EApp a e1 e2) = pretty e1 <+> pretty e2
    pretty (ELet a varName def expr) =
        "let" <+> pretty varName <+> "="
        <+> pretty def <+> "in" <+> pretty expr
--    pretty (EAsc a t e) = parens $ pretty e <+> "::" <+> pretty t
    pretty (EGetField a e name) = pretty e <> "#" <> pretty name

instance Pretty (f (Fix f)) => Pretty (Fix f) where
    pretty (Fix f) = pretty f

instance Pretty Class where
    pretty (Class (Id name) k) = pretty name

instance Pretty t => Pretty (Pred t) where
    pretty (PredIs c t) = pretty c <+> pretty t
    pretty (PredNoLabel c t) = pretty t <> "/" <> pretty c

instance (Pretty t) => Pretty (TVarLink t) where
    pretty (Unbound n l) = numToLetter n <> "'" -- "<unbound:" <+> pretty n <> "," <+> "level:" <+> pretty l <> ">"
    pretty (Link t) = "=" <> pretty t

instance (Pretty (v (TVarLink t))) => Pretty (TypeVar v t) where
    pretty (TypeVar cell k) = parens $ pretty cell <+> "::" <+> pretty k

instance (HasKind t, Pretty (v (TVarLink t)), Pretty t) => Pretty (TypeABT v t) where
    pretty (TyVar v) = pretty v
    pretty (TyAST t) = pretty t

-- instance Pretty (STRef s a) where
--     pretty _ = "<cell>"

-- instance Pretty (SType s) where
--     pretty (SType t) = pretty t

instance Pretty PType where
    pretty (PType t) = pretty t

instance Pretty a => Pretty (PCell a) where
    pretty (PCell x) = pretty x

instance Pretty t => Pretty (QualType t) where
    pretty (QualType [] t) = pretty t
    pretty (QualType ps t) = pretty ps <+> "=>" <+> pretty t

instance (Pretty e, Pretty a) => Pretty (Either e a) where
    pretty (Left e) = "Error:" <+> pretty e
    pretty (Right a) = pretty a

instance Pretty TypeError where
    pretty (WrappedError eOuter eInner) = align $ vsep [pretty eOuter, "in", pretty eInner]
    pretty (UnificationError a b) = "Failed unifying:" <+> align (vsep [pretty a, "with", pretty b])
    pretty (RowEndError x) = "Trailing row remainder:" <+> pretty x
    pretty (InferenceError x) = "Failed inferring a type for expression:" <+> pretty x
    pretty (EscapedSkolemError x) = "Skolem escaped:" <+> pretty x
    pretty (InvalidKind) = "Invalid kind"
    pretty (KindMismatchError k1 k2) = "Kinds mismatch error:" <+> align (vsep [pretty k1, pretty k2])
    pretty (InvalidVarError x) = "Unknown variable:" <+> pretty x
    pretty (ExpectedFunction x) = "Expected function type, got:" <+> pretty x
