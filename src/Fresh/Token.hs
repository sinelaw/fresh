{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Token (Token(..), getAnnotation, getContent) where

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
