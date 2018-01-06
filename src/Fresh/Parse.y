{
module Fresh.Parse where

import Fresh.Lexer (Token(..))
import Data.Char (isSpace, isAlpha, isUpper, isLower, isAlphaNum, isDigit)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      ident           { TokenIdent $$ }
      constr          { TokenConstr $$ }
      union           { TokenTUnion }
      func            { TokenFunc }
      switch          { TokenSwitch }
      case            { TokenCase }
      return          { TokenReturn }
      lam             { TokenLam }
      op              { TokenOp $$ }
      var             { TokenVar }
      ':'             { TokenColon }
      '<'             { TokenTriangleOpen }
      '>'             { TokenTriangleClose }
      '('             { TokenParenOpen }
      ')'             { TokenParenClose }
      '{'             { TokenBraceOpen }
      '}'             { TokenBraceClose }
      '->'            { TokenArrow }
      ';'             { TokenSemi }
      ','             { TokenComma }
      '='             { TokenEq }
      '@'             { TokenAt }
      number          { TokenInt $$ }

%left op

%%

Stmts       : Stmt                              { [$1] }
            | Stmts Stmt                        { $2 : $1 }

StmtBlock   : '{' Stmts '}'                     { $2 }

StmtOrBlock : Stmt                              { [$1] }
            | StmtBlock                         { $1 }

Stmt        : var ident '=' Expr ';'            { StmtLetVar (VarName $2) $4 }
            | return ';'                        { StmtReturn Nothing }
            | return Expr ';'                   { StmtReturn (Just $2) }
            | Func                              { $1 }
            | TUnion                            { StmtType $1 }
            | Expr ';'                          { StmtExpr $1 }

FuncArgs    : {- empty -}                       { [] }
            | FuncArg                           { [$1] }
            | FuncArgs ',' FuncArg              { $3 : $1 }

Func        : func ident '(' FuncArgs ')' StmtBlock { StmtLetVar (VarName $2) (Lam $4 $6) }

TUnion      : union constr TUnionArgs '{' TUnionConstrs '}' { TUnion (TypeName $2) $3 $5 }

TUnionArgs  : '<' TUnionArgsNotEmpty '>'         { $2 }
            | {- empty -}                        { [] }

TUnionArgsNotEmpty : ident                       { [TVarName $1] }
                  | TUnionArgsNotEmpty ',' ident { (TVarName $3) : $1 }

TUnionConstrs : TUnionConstr                     { [$1] }
              | TUnionConstrs ',' TUnionConstr   { $3 : $1 }
              | TUnionConstrs ','                { $1 }

TUnionConstr  : constr '(' ConstrArgs ')'        { ConstrDef (ConstrName $1) $3 }
              | constr                           { ConstrDef (ConstrName $1) [] }

TypeSpec    : ident                             { TSVar (TVarName $1) }
            | constr                            { TSName (TypeName $1) }
            | TypeSpec '<' TypeSpecArgs '>'     { TSApp $1 $3 }

TypeSpecArgs  : TypeSpec                        { [$1] }
              | TypeSpecArgs ',' TypeSpec       { $3 : $1 }

ConstrArg   : ident ':' TypeSpec                { ConstrArg (VarName $1) $3 }

ConstrArgs  : ConstrArg                         { [$1] }
            | ConstrArgs ',' ConstrArg          { $3 : $1 }

PatternMatchConstrArgs : ident                            { [VarName $1] }
                       | PatternMatchConstrArgs ',' ident { (VarName $3) : $1 }

PatternMatchConstr : constr                     { PatternConstrAll (ConstrName $1) }
                   | constr '(' PatternMatchConstrArgs ')' { PatternConstrUnpack (ConstrName $1) $3 }

PatternMatch  : PatternMatchConstr              { PatternMatchAnon ($1) }
              | ident '@' PatternMatchConstr    { PatternMatchNamed (VarName $1) ($3) }
              | ident                           { PatternMatchAny   (VarName $1) }
              -- | ident ':'                       { PatternMatch (VarName $1) Nothing }
              -- | ident ':' constr                { PatternMatch (VarName $2) (Just $3) }

PatternMatches : PatternMatch                      { [$1] }
               | PatternMatches ',' PatternMatch   { $3 : $1 }

SwitchCase  : case PatternMatch ':' StmtOrBlock           { SwitchCase [$2] $4 }
            | case '(' PatternMatches ')' ':' StmtOrBlock { SwitchCase $3 $6 }

SwitchCases : SwitchCase                        { [$1] }
            | SwitchCases SwitchCase            { $2: $1 }

FuncArg  : ident ':' TypeSpec                   { FuncArg (VarName $1) (Just $3) }
         | ident                                { FuncArg (VarName $1) Nothing }

Switch      : switch Expr '{' SwitchCases '}' { Switch $2 $4 }

TupleArgs   : Expr                              { [$1] }
            | TupleArgs ',' Expr                { $3 : $1 }

Expr        : lam ident '->' Stmts              { Lam [FuncArg (VarName $2) Nothing] $4 }
            | lam '(' ident ':' TypeSpec ')' '->' Stmts
                                                { Lam [FuncArg (VarName $3) (Just $5)] $8 }
            | Expr '('  ')'                     { Call $1 [] }
            | Expr '(' TupleArgs ')'            { Call $1 $3 }
            | '(' TupleArgs ')'                 { Tuple $2 }
            | Expr op Expr                      { OpApp (Op $2) $1 $3 }
            | Expr '=' Expr                     { OpApp (Op "=") $1 $3 }
            | Switch                            { $1 }
            | ident                             { Var (VarName $1) }
            | constr                            { Constr (ConstrName $1) }
            | number                            { LitNum $1 }
{

parseError :: [Token] -> a
parseError ts = error $ "Parse error at: " ++ (show ts)


data Op = Op String
    deriving Show
data ConstrName = ConstrName String
    deriving Show
data VarName = VarName String
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
          | Constr ConstrName
          | Switch Expr [SwitchCase]
          | Return Expr
          | Tuple [Expr]
          | LitNum Int
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


}
