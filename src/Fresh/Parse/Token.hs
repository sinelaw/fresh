{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Parse.Token (Token(..), getAnnotation, getContent, debugToken) where

import Data.Foldable
import Control.Monad (void)

data Token a
    = TokenIdent a String
    | TokenTypeIdent a String
    | TokenConstr a String
    -- | TokenComment a String
    | TokenOp a String
    | TokenInt a Int
    | TokenTUnion a
    | TokenFunc a
    | TokenSwitch a
    | TokenCase a
    | TokenReturn a
    | TokenLam a
    | TokenVar a
    | TokenMut a
    | TokenColon a
    | TokenTriangleOpen a
    | TokenTriangleClose a
    | TokenParenOpen a
    | TokenParenClose a
    | TokenBraceOpen a
    | TokenBraceClose a
    | TokenArrow a
    | TokenComma a
    | TokenEq a
    | TokenDot a
    | TokenSemi a
    | TokenAt a
    deriving (Eq, Show, Functor, Foldable, Traversable)

getAnnotation :: Token a -> a
getAnnotation t = x
    where [x] = toList t

getContent :: Token a -> String
getContent (TokenIdent _ s)     = s
getContent (TokenTypeIdent _ s) = s
getContent (TokenConstr _ s)    = s
-- getContent (TokenComment _ s)   = s
getContent (TokenOp _ s)        = s
getContent t                    = error $ "Token has no content: " ++ show (void t)

debugToken :: Token a -> String
debugToken (TokenIdent _ str)     = str
debugToken (TokenTypeIdent _ str) = str
debugToken (TokenConstr _ str)    = str
-- debugToken (TokenComment _ str)   = str
debugToken (TokenOp _ str)        = str
debugToken (TokenInt _ n)         = show n
debugToken (TokenTUnion _)        = "union"
debugToken (TokenFunc _)          = "func"
debugToken (TokenSwitch _)        = "switch"
debugToken (TokenCase _)          = "case"
debugToken (TokenReturn _)        = "return"
debugToken (TokenLam _)           = "lam"
debugToken (TokenVar _)           = "var"
debugToken (TokenMut _)           = "mut"
debugToken (TokenColon _)         = ":"
debugToken (TokenTriangleOpen _)  = "<"
debugToken (TokenTriangleClose _) = ">"
debugToken (TokenParenOpen _)     = "("
debugToken (TokenParenClose _)    = ")"
debugToken (TokenBraceOpen _)     = "["
debugToken (TokenBraceClose _)    = "]"
debugToken (TokenArrow _)         = "->"
debugToken (TokenComma _)         = ","
debugToken (TokenEq _)            = "="
debugToken (TokenDot _)           = "."
debugToken (TokenSemi _)          = ";"
debugToken (TokenAt _)            = "@"
