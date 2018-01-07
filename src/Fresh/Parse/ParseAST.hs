{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Parse.ParseAST where

data Op a = Op a String
    deriving (Show, Functor, Foldable, Traversable)
data ConstrName a = ConstrName a String
    deriving (Show, Functor, Foldable, Traversable)
data VarName a = VarName a String
    deriving (Show, Functor, Foldable, Traversable)
data FieldName a = FieldName a String
    deriving (Show, Functor, Foldable, Traversable)
data TVarName a = TVarName a String
    deriving (Show, Functor, Foldable, Traversable)

data TypeSpec a
    = TSVar a (TVarName a)
    | TSName a (TypeName a)
    | TSApp a (TypeSpec a) [TypeSpec a]
    deriving (Show, Functor, Foldable, Traversable)

getTypeSpecAnnotation :: TypeSpec a -> a
getTypeSpecAnnotation (TSVar a _) = a
getTypeSpecAnnotation (TSName a _) = a
getTypeSpecAnnotation (TSApp a _ _) = a

data TypeName a = TypeName a String
    deriving (Show, Functor, Foldable, Traversable)
data FuncArg a = FuncArg a (VarName a) (Maybe (TypeSpec a))
    deriving (Show, Functor, Foldable, Traversable)

data PatternConstr a = PatternConstrAll a (ConstrName a)
                     | PatternConstrUnpack a (ConstrName a) [VarName a]
    deriving (Show, Functor, Foldable, Traversable)

getPatternConstrAnnotation :: PatternConstr a -> a
getPatternConstrAnnotation (PatternConstrAll a _) = a
getPatternConstrAnnotation (PatternConstrUnpack a _ _) = a

data PatternMatch a = PatternMatchAll a
                    | PatternMatchAnon a (PatternConstr a)
                    | PatternMatchNamed a (VarName a) (PatternConstr a)
                    | PatternMatchAny a (VarName a)
    deriving (Show, Functor, Foldable, Traversable)

data SwitchCase a = SwitchCase a [PatternMatch a] [Stmt a]
    deriving (Show, Functor, Foldable, Traversable)

data Expr a
    = Lam a [FuncArg a] [Stmt a]
    | Call a (Expr a) [Expr a]
    | OpApp a (Op a) (Expr a) (Expr a)
    | Var a (VarName a)
    | Constr a (ConstrName a)
    | Switch a (Expr a) [SwitchCase a]
    | Return a (Expr a)
    | Tuple a [Expr a]
    | LitNum a Int
    | DotGet a (Expr a) (FieldName a)
    | Empty a
    deriving (Show, Functor, Foldable, Traversable)

getExprAnnotation :: Expr a -> a
getExprAnnotation (Lam a _ _) = a
getExprAnnotation (Call a _ _) = a
getExprAnnotation (OpApp a _ _ _) = a
getExprAnnotation (Var a _) = a
getExprAnnotation (Constr a _) = a
getExprAnnotation (Switch a _ _) = a
getExprAnnotation (Return a _) = a
getExprAnnotation (Tuple a _) = a
getExprAnnotation (LitNum a _) = a
getExprAnnotation (DotGet a _ _) = a
getExprAnnotation (Empty a) = a

data TUnion a
    = TUnion a (TypeName a) [TVarName a] [ConstrDef a]
    deriving (Show, Functor, Foldable, Traversable)

getTUnionAnnotation :: TUnion a -> a
getTUnionAnnotation (TUnion a _ _ _) = a

data ConstrDef a = ConstrDef a (ConstrName a) [ConstrArg a]
    deriving (Show, Functor, Foldable, Traversable)
data ConstrArg a = ConstrArg a (VarName a) (TypeSpec a)
    deriving (Show, Functor, Foldable, Traversable)

data Stmt a
    = StmtExpr a (Expr a)
    | StmtLetVar a (VarName a) (Expr a)
    | StmtType a (TUnion a)
    | StmtReturn a (Maybe (Expr a))
    | StmtVarSet a (VarName a) (Expr a)
    | StmtDotSet a (Expr a) (FieldName a) (Expr a)
    deriving (Show, Functor, Foldable, Traversable)

getStmtAnnotation :: Stmt a -> a
getStmtAnnotation (StmtExpr a _) = a
getStmtAnnotation (StmtLetVar a _ _) = a
getStmtAnnotation (StmtType a _) = a
getStmtAnnotation (StmtReturn a _) = a
getStmtAnnotation (StmtVarSet a _ _) = a
getStmtAnnotation (StmtDotSet a _ _ _) = a
