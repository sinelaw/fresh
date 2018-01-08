{
module Fresh.Parse.Parse where

import Fresh.Parse.ParseAST
import Fresh.Parse.Lexer (LToken, AlexPosn(..))
import Fresh.Parse.Token (Token(..))
import qualified Fresh.Parse.Token as Token
import Data.Char (isSpace, isAlpha, isUpper, isLower, isAlphaNum, isDigit)
import Data.List (intercalate)
}

%name parse
%tokentype { LToken }
%errorhandlertype explist
%error { parseError }

%token
      ident           { TokenIdent _ _ }
      constr          { TokenConstr _ _ }
      union           { TokenTUnion _ }
      func            { TokenFunc _ }
      switch          { TokenSwitch _ }
      case            { TokenCase _ }
      return          { TokenReturn _ }
      lam             { TokenLam _ }
      op              { TokenOp _ _ }
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
      number          { TokenInt _ _ }

%left op

%%

Stmts       : Stmt                              { [$1] }
            | Stmts Stmt                        { $2 : $1 }

StmtBlock   : '{' Stmts '}'                     { $2 }

StmtOrBlock : Stmt                              { [$1] }
            | StmtBlock                         { $1 }

Stmt        : var ident '=' Expr ';'            { StmtLetVar (gta $1) (VarName (gta $2) (gtc $2)) $4 }
            | return ';'                        { StmtReturn (gta $1) Nothing }
            | return Expr ';'                   { StmtReturn (gta $1) (Just $2) }
            | Func                              { $1 }
            | TUnion                            { StmtType (gua $1) $1 }
            | Expr '.' ident '=' Expr ';'       { StmtDotSet (gta $2) $1 (FieldName (gta $3) (gtc $3)) $5 }
            | ident '=' Expr ';'                { StmtVarSet (gta $1) (VarName (gta $1) (gtc $1)) $3 }
            | Expr ';'                          { StmtExpr (gea $1) $1 }

FuncArgs    : {- empty -}                       { [] }
            | FuncArg                           { [$1] }
            | FuncArgs ',' FuncArg              { $3 : $1 }

Func        : func ident '(' FuncArgs ')' StmtBlock { StmtLetVar (gta $1) (VarName (gta $2) (gtc $2)) (ExprLam (gta $3) $4 $6) }

TUnion      : union constr TUnionArgs '{' TUnionConstrs '}' { TUnion (gta $1) (TypeName (gta $2) (gtc $2)) $3 $5 }

TUnionArgs  : '<' TUnionArgsNotEmpty '>'         { $2 }
            | {- empty -}                        { [] }

TUnionArgsNotEmpty : ident                       { [TVarName (gta $1) (gtc $1)] }
                   | TUnionArgsNotEmpty ',' ident { (TVarName (gta $3) (gtc $3)) : $1 }

TUnionConstrs : TUnionConstr                     { [$1] }
              | TUnionConstrs ',' TUnionConstr   { $3 : $1 }
              | TUnionConstrs ','                { $1 }

TUnionConstr  : constr '(' ConstrArgs ')'        { ConstrDef (gta $1) (ConstrName (gta $1) (gtc $1)) $3 }
              | constr                           { ConstrDef (gta $1) (ConstrName (gta $1) (gtc $1)) [] }

TypeSpec    : ident                             { TSVar (gta $1) (TVarName (gta $1) (gtc $1)) }
            | constr                            { TSName (gta $1) (TypeName (gta $1) (gtc $1)) }
            | TypeSpec '<' TypeSpecArgs '>'     { TSApp (gtsa $1) $1 $3 }

TypeSpecArgs  : TypeSpec                        { [$1] }
              | TypeSpecArgs ',' TypeSpec       { $3 : $1 }

ConstrArg   : ident ':' TypeSpec                { ConstrArg (gta $1) (VarName (gta $1) (gtc $1)) $3 }

ConstrArgs  : ConstrArg                         { [$1] }
            | ConstrArgs ',' ConstrArg          { $3 : $1 }

PatternMatchConstrArgs : ident                            { [VarName (gta $1) (gtc $1)] }
                       | PatternMatchConstrArgs ',' ident { (VarName (gta $3) (gtc $3)) : $1 }

PatternMatchConstr : constr                     { PatternConstrAll (gta $1) (ConstrName (gta $1) (gtc $1)) }
                   | constr '(' PatternMatchConstrArgs ')' { PatternConstrUnpack (gta $1) (ConstrName (gta $1) (gtc $1)) $3 }

PatternMatch  : PatternMatchConstr              { PatternMatchAnon  (gpca $1) ($1) }
              | ident '@' PatternMatchConstr    { PatternMatchNamed (gta $1) (VarName (gta $1) (gtc $1)) ($3) }
              | ident                           { PatternMatchAny   (gta $1) (VarName (gta $1) (gtc $1)) }

PatternMatches : PatternMatch                      { [$1] }
               | PatternMatches ',' PatternMatch   { $3 : $1 }

SwitchCase  : case PatternMatch ':' StmtOrBlock           { SwitchCase (gta $1) $2 $4 }

SwitchCases : SwitchCase                        { [$1] }
            | SwitchCases SwitchCase            { $2: $1 }

FuncArg  : ident ':' TypeSpec                   { FuncArg (gta $1) (VarName (gta $1) (gtc $1)) (Just $3) }
         | ident                                { FuncArg (gta $1) (VarName (gta $1) (gtc $1)) Nothing }

Switch      : switch Expr '{' SwitchCases '}'   { ExprSwitch (gta $1) $2 $4 }

TupleArgs   : Expr                              { [$1] }
            | TupleArgs ',' Expr                { $3 : $1 }

Expr        : lam ident '->' Stmts              { ExprLam (gta $1) [FuncArg (gta $2) (VarName (gta $2) (gtc $2)) Nothing] $4 }
            | lam '(' ident ':' TypeSpec ')' '->' Stmts
                                                { ExprLam (gta $1) [FuncArg (gta $2) (VarName (gta $3) (gtc $3)) (Just $5)] $8 }
            | Expr '('  ')'                     { ExprCall (gta $2) $1 [] }
            | Expr '(' TupleArgs ')'            { ExprCall (gta $2) $1 $3 }
            | '(' TupleArgs ')'                 { ExprTuple (gta $1) $2 }
            | Expr op Expr                      { ExprOpApp (gta $2) (Op (gta $2) (gtc $2)) $1 $3 }
            | Expr '.' ident                    { ExprDotGet (gta $2) $1 (FieldName (gta $3) (gtc $3)) }
            | Switch                            { $1 }
            | ident                             { ExprVar (gta $1) (VarName (gta $1) (gtc $1)) }
            | constr                            { ExprConstr (gta $1) (ConstrName (gta $1) (gtc $1)) }
            | number                            { ExprLitNum (gta $1) (getNum $1) }
{

gta = Token.getAnnotation
gtc = Token.getContent
gsa = getStmtAnnotation
gea = getExprAnnotation
gua = getTUnionAnnotation
gtsa = getTypeSpecAnnotation
gpca = getPatternConstrAnnotation

getNum (TokenInt _ n) = n

parseError :: ([LToken], [String]) -> a
parseError (t, opts) = error $ pos ++ ": Got '" ++ tok ++ "' expected one of: " ++ (intercalate ", " opts)
    where pos = show line ++ ":" ++ show col
          (AlexPn _ line col) = Token.getAnnotation $ head t
          tok = Token.debugToken $ head t

}
