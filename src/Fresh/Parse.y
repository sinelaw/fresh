{
module Fresh.Parse where

import Fresh.ParseAST
import Fresh.Lexer (Token(..), LToken)
import Data.Char (isSpace, isAlpha, isUpper, isLower, isAlphaNum, isDigit)
}

%name parse
%tokentype { LToken }
%error { parseError }

%token
      ident           { TokenIdent _ $$ }
      constr          { TokenConstr _ $$ }
      union           { TokenTUnion _ }
      func            { TokenFunc _ }
      switch          { TokenSwitch _ }
      case            { TokenCase _ }
      return          { TokenReturn _ }
      lam             { TokenLam _ }
      op              { TokenOp _ $$ }
      var             { TokenVar _ }
      ':'             { TokenColon _ }
      '<'             { TokenTriangleOpen _ }
      '>'             { TokenTriangleClose _ }
      '('             { TokenParenOpen _ }
      ')'             { TokenParenClose _ }
      '{'             { TokenBraceOpen _ }
      '}'             { TokenBraceClose _ }
      '->'            { TokenArrow _ }
      ';'             { TokenSemi _ }
      ','             { TokenComma _ }
      '.'             { TokenDot _ }
      '='             { TokenEq _ }
      '@'             { TokenAt _ }
      number          { TokenInt _ $$ }

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
            | Expr '.' ident '=' Expr           { DotSet $1 (FieldName $3) $5 }
            | Expr '.' ident                    { DotGet $1 (FieldName $3) }
            | Switch                            { $1 }
            | ident                             { Var (VarName $1) }
            | ident '=' Expr                    { VarSet (VarName $1) $3 }
            | constr                            { Constr (ConstrName $1) }
            | number                            { LitNum $1 }
{

parseError :: [LToken] -> a
parseError ts = error $ "Parse error at: " ++ (show $ head ts)

}
