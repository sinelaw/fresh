module Fresh.ParseAST where

data Op = Op String
    deriving Show
data ConstrName = ConstrName String
    deriving Show
data VarName = VarName String
    deriving Show
data FieldName = FieldName String
    deriving Show
data TVarName = TVarName String
    deriving Show
data TypeSpec = TSVar TVarName
              | TSName TypeName
              | TSApp TypeSpec [TypeSpec]
    deriving Show
data TypeName = TypeName String
    deriving Show
data FuncArg = FuncArg VarName (Maybe TypeSpec)
    deriving Show

data PatternConstr = PatternConstrAll ConstrName
                   | PatternConstrUnpack ConstrName [VarName]
    deriving Show

data PatternMatch = PatternMatchAll
                  | PatternMatchAnon PatternConstr
                  | PatternMatchNamed VarName PatternConstr
                  | PatternMatchAny   VarName
    deriving Show

data SwitchCase = SwitchCase [PatternMatch] [Stmt]
    deriving Show

data Expr = Lam [FuncArg] [Stmt]
          | Call Expr [Expr]
          | OpApp Op Expr Expr
          | Var VarName
          | VarSet VarName Expr
          | Constr ConstrName
          | Switch Expr [SwitchCase]
          | Return Expr
          | Tuple [Expr]
          | LitNum Int
          | DotGet Expr FieldName
          | DotSet Expr FieldName Expr
          | Empty
    deriving Show

data TUnion = TUnion TypeName [TVarName] [ConstrDef]
    deriving Show
data ConstrDef = ConstrDef ConstrName [ConstrArg]
    deriving Show
data ConstrArg = ConstrArg VarName TypeSpec
    deriving Show

data Stmt = StmtExpr Expr
          | StmtLetVar VarName Expr
          | StmtType TUnion
          | StmtReturn (Maybe Expr)
    deriving Show
