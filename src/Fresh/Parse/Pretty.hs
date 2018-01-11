{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fresh.Parse.Pretty (Pretty(..)) where

import Text.PrettyPrint.ANSI.Leijen
import Fresh.Parse.ParseAST

import Prelude hiding ((<$>))

vbraced :: [Doc] -> Doc
vbraced xs
    = nest 4 (lbrace
              <$> vsep xs)
    <$> rbrace

instance Pretty (Op a) where
    pretty (Op _ s) = pretty s

instance Pretty (ConstrName a) where
    pretty (ConstrName _ s) = pretty s

instance Pretty (VarName a) where
    pretty (VarName _ s) = pretty s

instance Pretty (FieldName a) where
    pretty (FieldName _ s) = pretty s

instance Pretty (TVarName a) where
    pretty (TVarName _ s) = pretty s

triangleSep :: [Doc] -> Doc
triangleSep = encloseSep "<" ">" ","

instance Pretty (TypeSpec a) where
    pretty (TSVar _ tv) = pretty tv
    pretty (TSName _ tn) = pretty tn
    pretty (TSApp _ ts tss) = pretty ts <> triangleSep (map pretty tss)


instance Pretty (TypeName a) where
    pretty (TypeName _ s) = pretty s

instance Pretty (FuncArg a) where
    pretty (FuncArg _ v Nothing) = pretty v
    pretty (FuncArg _ v (Just t)) = pretty t <+> pretty v

instance Pretty (PatternConstr a) where
    pretty (PatternConstrAll _ c) = pretty c
    pretty (PatternConstrUnpack _ c vars) = pretty c <> tupled (map pretty vars)

instance Pretty (PatternMatch a) where
    pretty (PatternMatchAll _) = "_"
    pretty (PatternMatchAnon _ p) = pretty p
    pretty (PatternMatchNamed _ v p) = pretty v <> "@" <> pretty p
    pretty (PatternMatchAny _ v) = pretty v

instance Pretty (SwitchCase a) where
    pretty (SwitchCase _ p stmts) = "case" <+> pretty p <> colon <+> vbraced (map pretty stmts)

instance Pretty (Expr a) where
    pretty (ExprLam _ args stmts) = tupled (map pretty args)
        <+> "->"
        <+> vbraced (map pretty stmts)
    pretty (ExprCall _ (ExprConstr _ (ConstrName _ "()")) (CallFormPrefix args)) = tupled (map pretty args)
    pretty (ExprCall _ e form) =
        case form of
        CallFormPrefix args -> pretty e <> tupled (map pretty args)
        CallFormInfix e1 e2 -> pretty e1 <+> pretty e <+> pretty e2
    pretty (ExprVar _ v) = pretty v
    pretty (ExprConstr _ c) = pretty c
    pretty (ExprSwitch _ e cases) = "switch"
        <+> parens (pretty e)
        <+> vbraced (map pretty cases)
    pretty (ExprLitNum _ num) = pretty num
    pretty (ExprDotGet _ e f) = pretty e <> "." <> pretty f

emptyTupled :: [Doc] -> Doc
emptyTupled [] = empty
emptyTupled ts = tupled ts

instance Pretty (TUnion a) where
    pretty (TUnion _ tname tvars constrs) =
        "union" <+> pretty tname <> (case tvars of
                                       [] -> empty
                                       _ -> triangleSep (map pretty tvars))
        <+> vbraced (map (\x -> pretty x <> comma) constrs)

instance Pretty (ConstrDef a) where
    pretty (ConstrDef _ n args) = pretty n <> emptyTupled (map pretty args)

instance Pretty (ConstrArg a) where
    pretty (ConstrArg _ v t) = pretty t <+> pretty v

instance Pretty (Stmt a) where
    pretty (StmtExpr _ e) = pretty e <> ";"
    pretty (StmtLetVar _ v e) = "var" <+> pretty v <+> "=" <+> pretty e
    pretty (StmtMutVar _ v e) = "mut" <+> pretty v <+> "=" <+> pretty e
    pretty (StmtType _ t) = pretty t
    pretty (StmtReturn _ (Just e)) = "return" <+> pretty e <> ";"
    pretty (StmtReturn _ Nothing) = "return;"
    pretty (StmtVarSet _ v e) = pretty v <+> "=" <+> pretty e <> ";"
    pretty (StmtDotSet _ e1 f e2) = pretty e1 <> "." <> pretty f <+> "=" <+> pretty e2 <> ";"
