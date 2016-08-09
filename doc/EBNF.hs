{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module EBNF where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where
        lineCmnt = L.skipLineComment "#"
        blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

semi :: Parser String
semi = symbol ";"

symb :: Parser Char
symb = foldr (<|>) empty $ map char "[]{}()<>'=|.,;"

character :: Char -> Parser Char
character q = (char '\\' *> char q) <|> letterChar <|> digitChar <|> symb <|> char '_'

data Grammar = Grammar [Rule]
    deriving (Show)

data Rule = Rule String RHS
    deriving (Show)

data RHS
    = RHSIdent String
    | RHSTerm Terminal
    | RHSZeroOrOne RHS
    | RHSZeroOrMore RHS
    | RHSOr RHS RHS
    | RHSThen RHS RHS
    deriving (Show)

data Terminal = Terminal String
    deriving (Show)

identifier :: Parser [Char]
identifier = lexeme $ (:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_')

terminal :: Parser Terminal
terminal = lexeme $
    Terminal <$> ( char '\'' *> some (character '\'') <* char '\''
                   <|> char '"' *> some (character '"') <* char '"'
                 )

lhs :: Parser [Char]
lhs = identifier

rhs :: Parser RHS
rhs = RHSIdent <$> identifier
     <|> RHSTerm <$> terminal
     <|> RHSZeroOrOne <$> ( symbol "[" *> rhs <* symbol "]" )
     <|> RHSZeroOrMore <$> ( symbol "{" *> rhs <* symbol "}" )
     <|> ( symbol "(" *> rhs <* symbol ")" )
     <|> RHSOr <$> rhs <* symbol "|" <*> rhs
     <|> RHSThen <$> rhs <* symbol "," <*> rhs

rule :: Parser Rule
rule = Rule <$> lhs <* symbol "=" <*> rhs <* symbol ";"

grammar :: Parser [Rule]
grammar = many rule
