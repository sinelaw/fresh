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
      enum            { TokenTEnum }
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

%%

Stmt        : Expr                          { StmtExpr $1 }
            | TEnum                             { StmtType $1 }
            | return Expr                       { StmtReturn $2 }
            | Stmt ';' Stmt                      { StmtSeq $1 $3 }
            | Stmt ';'                          { $1 }

TEnum        : enum constr '{' TEnumConstrs '}' { TEnum (TypeName $2) $4 }

TEnumConstrs : {- empty -}                      { [] }
             | TEnumConstrs ',' TEnumConstr     { $3 : $1 }

TEnumConstr  : constr '(' ConstrArgs ')'        { ConstrDef (ConstrName $1) $3 }
             | constr                           { ConstrDef (ConstrName $1) [] }

ConstrArg   : ident ':' constr                  { ConstrArg (VarName $1) (TypeSpec $3)  }

ConstrArgs  : ConstrArg                         { [$1] }
            | ConstrArgs ',' ConstrArg          { $3 : $1 }

PatternMatch  : ident ':'                    { PatternMatch (VarName $1) Nothing }
              | '(' ident ':' constr ')'     { PatternMatch (VarName $2) (Just (ConstrName $4)) }

SwitchCase  : PatternMatch ':' Stmt             { ($1, $3) }

SwitchCases : SwitchCase                        { [$1] }
            | SwitchCases ',' SwitchCase        { $3 : $1 }

FuncArg  : ident ':' constr             { FuncArg (VarName $1) (Just (TypeSpec $3)) }
         | ident                        { FuncArg (VarName $1) Nothing }

FuncArgs : FuncArg                        { [$1] }
         | FuncArgs ',' FuncArg        { $3 : $1 }


Func        : func ident '(' FuncArgs ')' '{' Stmt '}' { Func (VarName $2) $4 $7 }

Switch      : switch '(' Expr ')' '{' SwitchCases '}' { Switch $3 $6 }

Expr        : lam ident '->' Expr               { Lam (VarName $2) $4 }
            | Expr '(' Expr ')'                 { App $1 $3 }
            | Expr op Expr                      { OpApp (Op $2) $1 $3 }
            | Switch                            { $1 }
            | Func                              { $1 }
            | ident                             { Var (VarName $1) }

{

parseError :: [Token] -> a
parseError ts = error $ "Parse error at: " ++ (show ts)


data Op = Op String
    deriving Show
data ConstrName = ConstrName String
    deriving Show
data VarName = VarName String
    deriving Show
data TypeSpec = TypeSpec String
    deriving Show
data TypeName = TypeName String
    deriving Show
data FuncArg = FuncArg VarName (Maybe TypeSpec)
    deriving Show

data PatternMatch = PatternMatch VarName (Maybe ConstrName)
    deriving Show

data Expr = Lam VarName Expr
          | App Expr Expr
          | OpApp Op Expr Expr
          | Var VarName
          | Switch Expr [(PatternMatch, Stmt)]
          | Func VarName [FuncArg] Stmt
    deriving Show

data TEnum = TEnum TypeName [ConstrDef]
    deriving Show
data ConstrDef = ConstrDef ConstrName [ConstrArg]
    deriving Show
data ConstrArg = ConstrArg VarName TypeSpec
    deriving Show

data Stmt = StmtExpr Expr
          | StmtType TEnum
          | StmtSeq Stmt Stmt
          | StmtReturn Expr
    deriving Show

data Token
    = TokenIdent String
    | TokenTypeIdent String
    | TokenConstr String
    | TokenTEnum
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
lexer ('/':'/':cs) = TokenComment com : lexer (tail ment)
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
      ("enum"   , rest) -> TokenTEnum    : lexer rest
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
