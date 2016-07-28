{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fresh.Parse where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import Fresh.CodeGen


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

-- | 'integer' parses an integer.

integer :: Parser Integer
integer = lexeme L.integer

-- | 'semi' parses a semicolon.

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","while"]

identifier :: Parser String
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

--whileParser :: Parser GStmt
-- whileParser = sc *> stmt <* eof

instance Monoid (GStmt () t) where
    mempty                              = GSBlock () mempty
    (GSBlock () xs) `mappend` (GSBlock () ys) = GSBlock () (xs  `mappend` ys)
    (GSBlock () xs) `mappend` y               = GSBlock () (xs  `mappend` [y])
    x               `mappend` (GSBlock () ys) = GSBlock () ([x] `mappend` ys)
    x               `mappend` y               = GSBlock () ([x] `mappend` [y])


stmt, stmtSeq, stmt' :: Parser (GStmt () t)
stmt    = parens stmt <|> stmtSeq
stmtSeq = mconcat <$> sepBy1 stmt' semi
stmt'   = ifStmt <|> whileStmt <|> assignStmt <|> scopeStmt

ifStmt, whileStmt, assignStmt, scopeStmt :: Parser (GStmt () t)
ifStmt     = GSIf ()               <$> (rword    "if" *> expr) <*> stmt' <*> (rword "else" *> stmt')
whileStmt  = GSWhile ()            <$> (rword "while" *> expr) <*> stmt'
assignStmt = GSVarWrite () . GName <$> identifier <* symbol "=" <*> expr
scopeStmt  = emptyScope            <$> braces stmt
    where emptyScope = GSScope () . GScope () []

expr :: Parser (GExpr () t)
expr = makeExprParser aTerm aOperators

uniop :: GExpr () t -> GExpr () t -> GExpr () t
uniop op x = GECall () op [x]

binop :: GExpr () t -> GExpr () t -> GExpr () t -> GExpr () t
binop op x y = GECall () op [x, y]

opn :: String -> GExpr () t
opn n = GELValue () (GLVarRead () (GName n))

aOperators :: [[Operator Parser (GExpr () t)]]
aOperators =
  [ [ Prefix (symbol "-" *> pure (uniop (opn "-"))) ]
  , [ InfixL (symbol "*" *> pure (binop (opn "*")))
    , InfixL (symbol "/" *> pure (binop (opn "/"))) ]
  , [ InfixL (symbol "+" *> pure (binop (opn "+")))
    , InfixL (symbol "-" *> pure (binop (opn "-"))) ]
  ]

aTerm :: Parser (GExpr () t)
aTerm = parens expr
     <|> GELValue () . GLVarRead () . GName <$> identifier
     <|> GELit () . GLitInt . fromInteger <$> integer

