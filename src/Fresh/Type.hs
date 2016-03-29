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

data GenVar = GenVar { _genVarId :: Id, _genVarKind :: Kind }
    deriving (Eq, Ord, Show)

data TypeAST t
    = TyAp { _tyApFun :: t, _tyApArg :: t }
    | TyCon { _tyCon :: TCon }
    | TyGenVar { _tyGenVar :: GenVar }
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

varBind :: STRef s (Maybe (SType s)) -> SType s -> ST s ()
varBind ref t = do
    vt <- readSTRef ref
    case vt of
        Nothing -> writeSTRef ref (Just t)
        Just t' -> unify t' t

unify :: SType s -> SType s -> ST s ()
unify (SType (TyVar ref)) t = varBind ref t
unify t (SType (TyVar ref)) = varBind ref t
unify (SType (TyAST t1)) (SType (TyAST t2)) = unifyAST t1 t2

unifyAST :: TypeAST (SType s) -> TypeAST (SType s) -> ST s ()
unifyAST (TyAp t1 t2) (TyAp t1' t2') = do
    unify t1 t1'
    unify t2 t2'
unifyAST (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
unifyAST (TyGenVar g1) (TyGenVar g2) | g1 == g2 = return ()
unifyAST _ _ = fail "oh no."

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

