{
module Fresh.Parse where

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
      ':'             { TokenColon }
      '('             { TokenParenOpen }
      ')'             { TokenParenClose }
      '{'             { TokenBraceOpen }
      '}'             { TokenBraceClose }
      '->'            { TokenArrow }
      ','             { TokenComma }
      ';'             { TokenSemi }

%left op

%%

Stmts       : {- empty -}                       { [] }
            | Stmt                              { [$1] }
            | Stmts ';' Stmt                    { $3 : $1 }
            | Stmts ';'                         { $1 }

Stmt        : Expr                              { StmtExpr $1 }
            | TUnion                             { StmtType $1 }
            | return Expr                       { StmtReturn $2 }

TUnion        : union constr TUnionArgs '{' TUnionConstrs '}' { TUnion (TypeName $2) $3 $5 }

TUnionArgs    : '(' TUnionArgsNotEmpty ')'        { $2 }
             | {- empty -}                      { [] }

TUnionArgsNotEmpty : ident                       { [TVarName $1] }
                  | TUnionArgsNotEmpty ',' ident { (TVarName $3) : $1 }

TUnionConstrs : TUnionConstr                      { [$1] }
             | TUnionConstrs ',' TUnionConstr     { $3 : $1 }
             | TUnionConstrs ','                 { $1 }

TUnionConstr  : constr '(' ConstrArgs ')'        { ConstrDef (ConstrName $1) $3 }
             | constr                           { ConstrDef (ConstrName $1) [] }

TypeSpec    : ident                             { TSVar (TVarName $1) }
            | constr                            { TSName (TypeName $1) }
            | TypeSpec '(' TypeSpecArgs ')'     { TSApp $1 $3 }

TypeSpecArgs  : TypeSpec                        { [$1] }
              | TypeSpecArgs ',' TypeSpec       { $3 : $1 }

ConstrArg   : ident ':' TypeSpec                { ConstrArg (VarName $1) $3 }

ConstrArgs  : ConstrArg                         { [$1] }
            | ConstrArgs ',' ConstrArg          { $3 : $1 }

PatternMatch  : constr                          { PatternMatch (Just (ConstrName $1)) }
              -- | ident ':'                       { PatternMatch (VarName $1) Nothing }
              -- | ident ':' constr                { PatternMatch (VarName $2) (Just $3) }

PatternMatches : PatternMatch                      { [$1] }
               | PatternMatches ',' PatternMatch   { $3 : $1 }

SwitchCaseStmts : Stmt                             { [$1] }
                | '{' Stmts '}'                    { $2 }

SwitchCase  : case PatternMatch ':' SwitchCaseStmts             { SwitchCase [$2] $4 }
            | case '(' PatternMatches ')' ':' SwitchCaseStmts   { SwitchCase $3 $6 }

SwitchCases : SwitchCase                        { [$1] }
            | SwitchCases ';' SwitchCase            { $3 : $1 }
            | SwitchCases ';'                   { $1 }

FuncArg  : ident ':' TypeSpec                   { FuncArg (VarName $1) (Just $3) }
         | ident                                { FuncArg (VarName $1) Nothing }

FuncArgs : {- empty -}                          { [] }
         | FuncArg                              { [$1] }
         | FuncArgs ',' FuncArg                 { $3 : $1 }


Func        : func ident '(' FuncArgs ')' '{' Stmts '}' { Func (VarName $2) $4 $7 }

Switch      : switch '(' Exprs ')' '{' SwitchCases '}' { Switch $3 $6 }

Exprs       : Expr                              { [$1] }
            | Exprs ',' Expr                    { $3 : $1 }

Expr        : lam ident '->' Expr               { Lam (VarName $2) $4 }
            | Expr '('  ')'                     { Call $1 [] }
            | Expr '(' Exprs ')'                { Call $1 $3 }
            | Expr op Expr                      { OpApp (Op $2) $1 $3 }
            | Switch                            { $1 }
            | Func                              { $1 }
            | ident                             { Var (VarName $1) }
            | constr                            { Constr (ConstrName $1) }
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

data PatternMatch = PatternMatch (Maybe ConstrName)
    deriving Show

data SwitchCase = SwitchCase [PatternMatch] [Stmt]
    deriving Show

data Expr = Lam VarName Expr
          | Call Expr [Expr]
          | OpApp Op Expr Expr
          | Var VarName
          | Constr ConstrName
          | Switch [Expr] [SwitchCase]
          | Func VarName [FuncArg] [Stmt]
    deriving Show

data TUnion = TUnion TypeName [TVarName] [ConstrDef]
    deriving Show
data ConstrDef = ConstrDef ConstrName [ConstrArg]
    deriving Show
data ConstrArg = ConstrArg VarName TypeSpec
    deriving Show

data Stmt = StmtExpr Expr
          | StmtType TUnion
          | StmtReturn Expr
    deriving Show

data Token
    = TokenIdent String
    | TokenTypeIdent String
    | TokenConstr String
    | TokenTUnion
    | TokenFunc
    | TokenSwitch
    | TokenCase
    | TokenReturn
    | TokenLam
    | TokenColon
    | TokenParenOpen
    | TokenParenClose
    | TokenBraceOpen
    | TokenBraceClose
    | TokenArrow
    | TokenComma
    | TokenSemi
    | TokenComment String
    | TokenOp String
    | TokenInt Integer
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('(':cs) = TokenParenOpen  : lexer cs
lexer (')':cs) = TokenParenClose : lexer cs
lexer ('{':cs) = TokenBraceOpen  : lexer cs
lexer ('}':cs) = TokenBraceClose : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (';':cs) = TokenSemi  : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('/':'/':cs) = lexer (tail ment) -- TokenComment com : lexer (tail ment)
    where
      (com, ment) = break (== '\n') cs
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
      | otherwise = TokenOp [c] : lexer cs
lexer cs = error ("Unknown token: " ++ show cs)

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("union"  , rest) -> TokenTUnion  : lexer rest
      ("func"   , rest) -> TokenFunc    : lexer rest
      ("switch" , rest) -> TokenSwitch  : lexer rest
      ("case"   , rest) -> TokenCase    : lexer rest
      ("return" , rest) -> TokenReturn  : lexer rest
      ("lam"    , rest) -> TokenLam     : lexer rest
      (vs'       , rest') -> case span isAlphaNum cs of
        ((v:vs), rest) | isUpper v -> TokenConstr (v:vs) : lexer rest
        ((v:vs), rest) | isLower v -> TokenIdent (v:vs)  : lexer rest

main = getContents >>= print . parse . lexer
}
