{
module Fresh.Parse where

import Data.Char (isSpace, isAlpha, isUpper, isLower)
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
            | Stmt ';' Stmt                      { StmtSeq $1 $3 }
            | Stmt ';'                          { $1 }

TEnum        : enum constr '{' '}'              { TEnum $2 [] }
             | enum constr '{' TEnumConstrs '}' { TEnum $2 $4 }

TEnumConstrs : TEnumConstr                      { [$1] }
             | TEnumConstrs ',' TEnumConstr     { $3 : $1 }

TEnumConstr  : constr '(' ConstrArgs ')'        { ConstrDef $1 $3 }
             | constr                           { ConstrDef $1 [] }

ConstrArg   : ident ':' constr                  { ConstrArg $1 $3  }

ConstrArgs  : ConstrArg                         { [$1] }
            | ConstrArgs ',' ConstrArg          { $3 : $1 }

Expr        : lam ident '->' Expr               { Lam $2 $4 }
            | Expr '(' Expr ')'                 { App $1 $3 }
            | ident                             { Var $1 }

{

parseError :: [Token] -> a
parseError ts = error $ "Parse error at: " ++ (show ts)


data Expr = Lam String Expr
          | App Expr Expr
          | Var String
    deriving Show

data TEnum = TEnum String [ConstrDef]
    deriving Show
data ConstrDef = ConstrDef String [ConstrArg]
    deriving Show
data ConstrArg = ConstrArg String String
    deriving Show

data Stmt = StmtExpr Expr
          | StmtType TEnum
          | StmtSeq Stmt Stmt
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
    -- | TokenComment */
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      -- | isDigit c = lexNum (c:cs) */
lexer ('(':cs) = TokenParenOpen  : lexer cs
lexer (')':cs) = TokenParenClose : lexer cs
lexer ('{':cs) = TokenBraceOpen  : lexer cs
lexer ('}':cs) = TokenBraceClose : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (';':cs) = TokenSemi  : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
-- lexNum cs = TokenInt (read num) : lexer rest */
--       where (num,rest) = span isDigit cs */

lexVar cs =
   case span isAlpha cs of
      ("enum"   , rest) -> TokenTEnum    : lexer rest
      ("func"   , rest) -> TokenFunc    : lexer rest
      ("switch" , rest) -> TokenSwitch  : lexer rest
      ("case"   , rest) -> TokenCase    : lexer rest
      ("return" , rest) -> TokenReturn  : lexer rest
      ("lam"    , rest) -> TokenLam     : lexer rest
      ((v:vs)   , rest) | isUpper v -> TokenConstr (v:vs) : lexer rest
      ((v:vs)   , rest) | isLower v -> TokenIdent (v:vs) : lexer rest

main = getContents >>= print . parse . lexer
}
