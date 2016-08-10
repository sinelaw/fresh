{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module EBNF where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import System.IO (readFile)
import System.Environment (getArgs)

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

data Op = OpChoice | OpThen
    deriving (Show)

data RHS
    = RHSIdent String
    | RHSTerm Terminal
    | RHSZeroOrOne RHS
    | RHSZeroOrMore RHS
    | RHSBinary Op [RHS]
    deriving (Show)

data Terminal = Terminal String
    deriving (Show)

identifier :: Parser [Char]
identifier = label "identifier" $ lexeme $
    (:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_')

terminal :: Parser Terminal
terminal = label "terminal" $ lexeme $
    Terminal <$> ( char '\'' *> some (character '\'') <* char '\''
                   <|> char '"' *> some (character '"') <* char '"'
                 )

lhs :: Parser [Char]
lhs = identifier

rhsPair :: Parser RHS
rhsPair = do
    r <- rhs
    op <- (pure OpChoice <* symbol "|") <|> (pure OpThen <* symbol ",")
    let opParser =
            case op of
                OpChoice -> symbol "|"
                OpThen -> symbol ","
    RHSBinary op . (r:) <$> ((:) <$> rhs <*> many (opParser *> rhs))

rhs :: Parser RHS
rhs = RHSZeroOrOne <$> ( symbol "[" *> rhs <* symbol "]" )
      <|> RHSZeroOrMore <$> ( symbol "{" *> rhs <* symbol "}" )
      <|> RHSTerm <$> terminal
      <|> RHSIdent <$> identifier
      <|> ( symbol "(" *> (rhsPair <?> "expression") <* symbol ")" )

rule :: Parser Rule
rule = Rule <$> lhs <* symbol "=" <*> rhs <* symbol ";"

grammar :: Parser [Rule]
grammar = many rule <* eof

main :: IO ()
main = (head <$> getArgs) >>= readFile >>= parseTest grammar
