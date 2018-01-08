{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Parse.Token (Token(..), getAnnotation, getContent, debugToken) where

import Data.Foldable

data Token a
    = TokenIdent a String
    | TokenTypeIdent a String
    | TokenConstr a String
    | TokenComment a String
    | TokenOp a String
    | TokenInt a Int
    | TokenTUnion a
    | TokenFunc a
    | TokenSwitch a
    | TokenCase a
    | TokenReturn a
    | TokenLam a
    | TokenVar a
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
getContent (TokenIdent _ s) = s
getContent (TokenTypeIdent _ s) = s
getContent (TokenConstr _ s) = s
getContent (TokenComment _ s) = s
getContent (TokenOp _ s) = s

debugToken :: Token a -> String
debugToken (TokenIdent _ str) = str
debugToken (TokenTypeIdent _ str) = str
debugToken (TokenConstr _ str) = str
debugToken (TokenComment _ str) = str
debugToken (TokenOp _ str) = str
debugToken (TokenInt _ n) = show n
debugToken (TokenTUnion _) = "TUnion"
debugToken (TokenFunc _) = "Func"
debugToken (TokenSwitch _) = "Switch"
debugToken (TokenCase _) = "Case"
debugToken (TokenReturn _) = "Return"
debugToken (TokenLam _) = "Lam"
debugToken (TokenVar _) = "Var"
debugToken (TokenColon _) = "Colon"
debugToken (TokenTriangleOpen _) = "TriangleOpen"
debugToken (TokenTriangleClose _) = "TriangleClose"
debugToken (TokenParenOpen _) = "ParenOpen"
debugToken (TokenParenClose _) = "ParenClose"
debugToken (TokenBraceOpen _) = "BraceOpen"
debugToken (TokenBraceClose _) = "BraceClose"
debugToken (TokenArrow _) = "Arrow"
debugToken (TokenComma _) = "Comma"
debugToken (TokenEq _) = "Eq"
debugToken (TokenDot _) = "Dot"
debugToken (TokenSemi _) = "Semi"
debugToken (TokenAt _) = "At"
