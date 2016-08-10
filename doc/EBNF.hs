{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module EBNF where

import Data.List (intersperse)
import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import System.Environment (getArgs)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), char, empty)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where
        lineCmnt = L.skipLineComment "#"
        blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

eparens, ebraces, ebrackets :: Parser a -> Parser a
eparens   = between (symbol "(") (symbol ")")
ebraces   = between (symbol "{") (symbol "}")
ebrackets = between (symbol "[") (symbol "]")

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
    | RHSChoice RHS [RHS]
    | RHSSeq RHS [RHS]
    deriving (Show)

data Terminal = Terminal String
    deriving (Show)

mkList c r [] = r
mkList c r rs = c r rs

instance Pretty Grammar where
    pretty (Grammar rules) = vsep (map pretty rules)

instance Pretty Rule where
    pretty (Rule l r) = pretty l <+> "=" <+> pretty r <+> ";"

instance Pretty RHS where
    pretty (RHSIdent i) = pretty i
    pretty (RHSTerm  t) = pretty t
    pretty (RHSZeroOrOne  r) = "[" <+> pretty r <+> "]"
    pretty (RHSZeroOrMore r) = "{" <+> pretty r <+> "}"
    pretty (RHSChoice r rs) = align . parens . vsep $ pretty r : (map (\r' -> "|" <+> pretty r') rs)
    pretty (RHSSeq    r rs) = align . parens . hsep $ pretty r : (map (\r' -> "," <+> pretty r') rs)

instance Pretty Terminal where
    pretty (Terminal s) = "\"" <> pretty s <> "\""


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

rhsSingle :: Parser RHS
rhsSingle = RHSZeroOrOne <$> ( symbol "[" *> rhs <* symbol "]" )
            <|> RHSZeroOrMore <$> ( symbol "{" *> rhs <* symbol "}" )
            <|> RHSTerm <$> terminal
            <|> RHSIdent <$> identifier
            <|> eparens rhs

rhsChoice :: Parser RHS
rhsChoice = mkList RHSChoice <$> rhsSeq <*> many (symbol "|" *> rhsSeq)

rhsSeq :: Parser RHS
rhsSeq = mkList RHSSeq <$> rhsSingle <*> many (symbol "," *> rhsSingle)

rhs :: Parser RHS
rhs = rhsChoice

rule :: Parser Rule
rule = Rule <$> lhs <* symbol "=" <*> rhs <* symbol ";"

grammar :: Parser Grammar
grammar = Grammar <$> many rule <* eof


main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    case parse grammar fileName contents of
        Left e -> putStrLn (show e)
        Right g -> do
            print g
            putStrLn $ show $ pretty g