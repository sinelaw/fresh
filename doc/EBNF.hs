{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module EBNF where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Char
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

-- helper for not building RHSChoice / RHSSeq that have just one element
mkList :: (b -> [a] -> b) -> b -> [a] -> b
mkList _ r [] = r
mkList c r rs = c r rs

symb :: Parser Char
symb = foldr (<|>) empty $ map char "[]{}()<>'=|.,;"

character :: Char -> Parser Char
character q = (char '\\' *> char q) <|> letterChar <|> digitChar <|> symb <|> char '_'

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


-- Pretty printing instances

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

------------------------------------------------------------------------

newtype CodeDoc = CodeDoc Doc

class Code a where
    code :: a -> Doc

snake :: String -> String
snake []         = []
snake ('_':x:xs) = Data.Char.toUpper x : snake xs
snake (  x:xs)   = x : snake xs

camel :: String -> String
camel []     = []
camel (x:xs) = Data.Char.toUpper x : snake xs

instance Code Grammar where
    code (Grammar rules) = vsep (map code rules)

instance Code Rule where
    code (Rule l r) = pretty (snake l) <+> "=" <+> code r

instance Code RHS where
    code (RHSIdent i) = pretty $ snake i
    code (RHSTerm  t) = code t
    code (RHSZeroOrOne  r) = parens $ "optional" <+> code r
    code (RHSZeroOrMore r) = parens $ "many" <+> code r
    code (RHSChoice r rs) = align . parens . vsep $ code r : (map (\r' -> "<|>" <+> code r') rs)
    code (RHSSeq    r rs) = align . parens . hsep $ code r : (map (\r' -> "<$>" <+> code r') rs)

instance Code Terminal where
    code (Terminal s) = pretty $ show s

------------------------------------------------------------------------

main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    case parse grammar fileName contents of
        Left e -> putStrLn (show e)
        Right g -> do
            print g
            putStrLn "----------------------------------------------------------------------"
            putStrLn $ show $ pretty g
            putStrLn "----------------------------------------------------------------------"
            putStrLn $ show $ code g
            
