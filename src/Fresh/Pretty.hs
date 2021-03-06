{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fresh.Pretty
       ( Pretty(..) )
       where

import Data.STRef (STRef)
import Text.PrettyPrint.ANSI.Leijen
import Fresh.Types
import Fresh.Kind (Kind(..))
import Fresh.Expr

import qualified Data.Map as Map

numToLetter :: Int -> Doc
numToLetter idx =
    if idx < length ['a'..'z']
    then char $ ['a'..'z'] !! idx
    else "t" <> int idx

instance Pretty TCon where
    pretty (TCon (Id s) _) = pretty s

instance Pretty Kind where
    pretty (KArrow k1 k2) = pretty k1 <+> "->" <+> pretty k2
    pretty Star           = "*"
    pretty Composite      = "@"

class LevelPretty l where
    levelPretty :: l -> Doc

instance LevelPretty () where
    levelPretty = const empty

instance LevelPretty Level where
    -- pretty _ = empty
    levelPretty LevelAny  = "^^"
    levelPretty (Level x) = "^" <> pretty x

instance LevelPretty g => Pretty (GenVar g) where
    pretty (GenVar idx k l) = pk name <> levelPretty l
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
    pretty CompositeTerminal      = empty
    pretty (CompositeRemainder t) = " |" <+> pretty t

instance (LevelPretty g, HasKind t, Pretty t) => Pretty (TypeAST g t) where
    pretty (TyAp fun arg) =
        case kind fun of
             Just (KArrow _ KArrow{}) -> pretty arg <+> pretty fun
                 -- TODO red for bad kind app
             _ -> parens $ pretty fun <+> pretty arg
    pretty (TyCon con)        = pretty con
    pretty (TyGenVar genVar)  = pretty genVar
    pretty (TyGen genVar t)   = parens $ "forall" <+> foldr ((<+>) . pretty) empty genVar <> "." <+> pretty t
    -- TODO
    pretty (TyComp c)         = "{" <+> pretty c <+> "}"

instance Pretty EVarName where
    pretty (EVarName s) = text s

instance Pretty (Lit a) where
    pretty (LitNum x)     = pretty x
    pretty (LitString x)  = dquotes $ pretty x
    pretty (LitBool x)    = pretty x
    pretty (LitStruct fs) = "{" <+> (vsep $ map (\(fname, fexpr) -> pretty fname <+> "=" <+> pretty fexpr) fs) <+> "}"

instance Pretty ETypeAsc where
    pretty (ETypeAsc t) = pretty t

instance Pretty (Expr a) where
    pretty (ELit       _a l)            = pretty l
    pretty (EVar       _a varName)      = pretty varName
    pretty (ELam       _a varName e)    = parens $ "\\" <> pretty varName <+> "->" <+> pretty e
    pretty (EALam      _a varName t e)  = parens $ "\\" <> parens (pretty varName <+> "::" <+> pretty t) <+> "->" <+> pretty e
    pretty (EApp       _a e1 e2)        = pretty e1 <+> pretty e2
    pretty (ELet       _a varName def expr) = "let" <+> pretty varName <+> "=" <+> pretty def <+> "in" <+> pretty expr
    pretty (EAsc       _a t e)          = parens $ pretty e <+> "::" <+> pretty t
    pretty (EGetField  _a e name)       = pretty e <> "#" <> pretty name
    pretty (EBuiltIn   _a name t)       = parens $ pretty name <+> "::" <+> pretty t

instance Pretty (f (Fix f)) => Pretty (Fix f) where
    pretty (Fix f) = pretty f

instance Pretty ClassId where
    pretty (ClassId name) = pretty name

instance Pretty MemberName where
    pretty (MemberName m) = pretty m

instance (Pretty t, Pretty e) => Pretty (Class t e) where
    pretty cls =
        vsep
        [ "Class"
            <+> pretty (map (\sup -> pretty sup <+> pretty (clsParam cls)) (clsSupers cls))
            <+> "=>"
            <+> pretty (clsId cls) <+> pretty (clsParam cls)
            <+> "where"
        , indent 4 $ vsep $ map (\(mem, t) -> pretty mem <+> "::" <+> pretty t) $ Map.toList (clsMembers cls)
        , vsep $ map pretty (clsInstances cls)
        ]

instance (Pretty t, Pretty e) => Pretty (Instance t e) where
    pretty (Instance cid t mems) =
        vsep
        [ "instance" <+> pretty cid <+> pretty t <+> "where"
        , indent 4 $ vsep $ map (\(mname, mt) -> pretty mname <+> "=" <+> pretty mt) $ Map.toList mems
        ]

instance Pretty t => Pretty (Pred t) where
    pretty (PredIs c t)      = pretty c <+> pretty t
    pretty (PredNoLabel c t) = pretty t <> "/" <> pretty c

instance (Pretty t) => Pretty (TVarLink t) where
    pretty (Unbound n l) = numToLetter n <> "'" -- "<unbound:" <+> pretty n <> "," <+> "level:" <+> pretty l <> ">"
    pretty (Link t)      = "=" <> pretty t

instance (Pretty (v (TVarLink t))) => Pretty (TypeVar v t) where
    pretty (TypeVar cell k) = parens $ pretty cell <+> "::" <+> pretty k

instance (LevelPretty g, HasKind t, Pretty (v (TVarLink t)), Pretty t) => Pretty (TypeABT g v t) where
    pretty (TyVar v) = pretty v
    pretty (TyAST t) = pretty t

instance Pretty (STRef s a) where
    pretty _ = "<cell>"

instance Pretty (SType s) where
    pretty (SType t) = pretty t

instance Pretty PType where
    pretty (PType t) = pretty t

instance Pretty a => Pretty (PCell a) where
    pretty (PCell x) = pretty x

instance Pretty t => Pretty (QualType t) where
    pretty (QualType [] t) = pretty t
    pretty (QualType ps t) = pretty ps <+> "=>" <+> pretty t

instance (Pretty e, Pretty a) => Pretty (Either e a) where
    pretty (Left e)  = "Error:" <+> pretty e
    pretty (Right a) = pretty a

instance Pretty TypeError where
    pretty (WrappedError eOuter eInner) = align $ vsep [pretty eOuter, "in", pretty eInner]
    pretty (ResolveError s)             = "Error while resolving:" <+> pretty s
    pretty (UnificationError a b)       = "Failed unifying:" <+> align (vsep [pretty a, "with", pretty b])
    pretty (RowEndError x)              = "Trailing row remainder:" <+> pretty x
    pretty (InferenceError x)           = "Failed inferring a type for expression:" <+> pretty x
    pretty (EscapedSkolemError x)       = "Skolem escaped:" <+> pretty x
    pretty InvalidKind                  = "Invalid kind"
    pretty (KindMismatchError k1 k2)    = "Kinds mismatch error:" <+> align (vsep [pretty k1, pretty k2])
    pretty (InvalidVarError x)          = "Unknown variable:" <+> pretty x
    pretty (ExpectedFunction x)         = "Expected function type, got:" <+> pretty x
    pretty (SubsumeError t1 t2)         = "Subsuming" <+> text t1 <+> "into" <+> text t2
    pretty (OccursError t1 t2)          = "Occurs check failed, " <+> text t1 <+> " is in " <+> text t2
    pretty (AssertionError s)           = "ASSERTION FAILED:" <+> text s
    pretty (MultipleErrors es)          = "Errors:" <+> align (vsep $ map pretty es)
    pretty (InstanceMethodMissing s)    = "Instance method missing:" <+> pretty s
    pretty (InstanceMemberWrongType s)  = "Instance member wrong type:" <+> pretty s
    pretty (CallFrame s)                = "Frame:" <+> pretty s
