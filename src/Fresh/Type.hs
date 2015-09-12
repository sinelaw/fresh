{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Type where

import           Fresh.Kind (Kind)
--import qualified Fresh.Kind as Kind
import Data.STRef
import Control.Monad.ST (ST)

data Id = Id String
    deriving (Eq, Ord, Show)

data TCon = TCon Id Kind
    deriving (Eq, Ord, Show)

data TypeAST t
    = TyAp { _tyApFun :: t, _tyApArg :: t }
    | TyCon { _tyCon :: TCon }
    | TyGenVar { _tyGenVarId :: Id, _tyGenVarKind :: Kind }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeABT v t
    = TyVar v
    | TyAST (TypeAST t)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Fix f = Fix { unFix :: f (Fix f) }

data SType s = SType (TypeABT (STRef s (Maybe (SType s))) (SType s))
type Type = Fix TypeAST

resolve :: SType s -> ST s (Maybe Type)
resolve (SType (TyVar ref)) = do
    mt <- readSTRef ref
    maybe (return Nothing) resolve mt
resolve (SType (TyAST t)) = do
    mt <- traverse resolve t
    return . fmap Fix $ sequenceA mt

----------------------------------------------------------------------

data EVar = EVar String
    deriving (Eq, Ord, Show)

data Lit
    = LitNum Double
    | LitString String
    | LitBool Bool
    deriving (Eq, Ord, Show)

data Expr e
    = ELit Lit
    | ELam EVar e
    | EApp e e
    deriving (Eq, Ord, Show)

----------------------------------------------------------------------

