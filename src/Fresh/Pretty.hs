{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Fresh.Pretty
       ( Pretty(..), pretty )
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

instance Pretty GenVar where
    pretty (GenVar idx k) = pk name
        where name = if idx < length ['a'..'z']
                     then char $ ['a'..'z'] !! idx
                     else "t" <> int idx
              pk = if k == Star
                   then id
                   else \x -> x <+> "::" <+> pretty k

instance Pretty t => Pretty (TypeAST t) where
    pretty (TyAp fun arg) =
        case show (pretty fun) of
            (n:ame) | Char.isAlpha n ->
                          parens $ pretty fun <+> pretty arg
            _ -> pretty arg <+> pretty fun
    pretty (TyCon con) = pretty con
    pretty (TyGenVar genVar) = pretty genVar
    pretty (TyGen genVars t) = "forall" <+> (foldr (<+>) "" $ map pretty genVars) <> "." <+> pretty t

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

instance Pretty (f (Fix f)) => Pretty (Fix f) where
    pretty (Fix f) = pretty f