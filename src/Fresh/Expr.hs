{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Fresh.Expr where


import GHC.Generics (Generic)
import qualified Data.Foldable
import Fresh.Types (QualType, Type, CompositeLabelName(..))

newtype ETypeAsc = ETypeAsc (QualType Type)
    deriving (Generic, Eq, Ord, Show)

data EVarName = EVarName String
    deriving (Generic, Eq, Ord, Show)

data Lit a
    = LitNum Double
    | LitString String
    | LitBool Bool
    | LitStruct [(CompositeLabelName, (Expr a))]
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

data Expr a
    = ELit a (Lit a)
    | EVar a EVarName
    | ELam a EVarName (Expr a)
    | EALam a EVarName ETypeAsc (Expr a)
    | EApp a (Expr a) (Expr a)
    | ELet a EVarName (Expr a) (Expr a)
    | EAsc a ETypeAsc (Expr a)
    | EGetField a (Expr a) CompositeLabelName
    | EBuiltIn a EVarName ETypeAsc
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

getAnnotation :: Expr a -> a
getAnnotation = head . Data.Foldable.toList

-- type FExpr = Fix Expr
--     deriving (Generic, Eq, Ord, Show)
