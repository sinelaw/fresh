{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fresh.Pretty
       ( Pretty(..) )
       where

import Text.PrettyPrint.ANSI.Leijen
import Fresh.Type
import Fresh.Kind (Kind(..))

import qualified Data.Char as Char

instance Pretty TCon where
    pretty (TCon (Id name) k) = text name

instance Pretty Kind where
    pretty (KArrow k1 k2) = pretty k1 <+> "->" <+> pretty k2
    pretty Star = "*"
    pretty Composite = "@"

instance Pretty GenVar where
    pretty (GenVar idx k) = pk name
        where name = if idx < length ['a'..'z']
                     then char $ ['a'..'z'] !! idx
                     else "t" <> int idx
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

instance Pretty t => Pretty (TypeAST t) where
    pretty (TyAp fun arg) =
        case show (pretty fun) of
            (n:ame) | Char.isAlpha n ->
                          parens $ pretty fun <+> pretty arg
            _ -> pretty arg <+> pretty fun
    pretty (TyCon con) = pretty con
    pretty (TyGenVar genVar) = pretty genVar
    pretty (TyGen genVars t) = "forall" <+> foldr (<+>) empty (map pretty genVars) <> "." <+> pretty t
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
    pretty (EApp a e1 e2) = pretty e1 <+> pretty e2
    pretty (ELet a varName def expr) =
        "let" <+> pretty varName <+> "="
        <+> pretty def <+> "in" <+> pretty expr
    pretty (EAsc a t e) = parens $ pretty e <+> "::" <+> pretty t

instance Pretty (f (Fix f)) => Pretty (Fix f) where
    pretty (Fix f) = pretty f

instance Pretty Class where
    pretty (Class (Id name) k) = pretty name

instance Pretty t => Pretty (Pred t) where
    pretty (PredIs c t) = pretty c <+> pretty t

instance Pretty t => Pretty (QualType t) where
    pretty (QualType [] t) = pretty t
    pretty (QualType ps t) = pretty ps <+> "=>" <+> pretty t

instance Pretty TypeError where
    pretty = text . show

instance (Pretty e, Pretty a) => Pretty (Either e a) where
    pretty (Left e) = "Error:" <+> pretty e
    pretty (Right a) = pretty a

