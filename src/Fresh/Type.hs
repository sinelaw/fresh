{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Type where

import           Fresh.Kind (Kind(..))
--import qualified Fresh.Kind as Kind
import Data.STRef
import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State (StateT(..), runStateT, modify, get, put, evalStateT)

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

tyFunc :: TypeAST t
tyFunc = TyCon (TCon (Id "->") (KArrow Star Star))

data TypeABT v t
    = TyVar v
    | TyAST (TypeAST t)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)

data SType s = SType (TypeABT (STRef s (Maybe (SType s))) (SType s))

type Type = Fix TypeAST

funT :: SType s -> SType s -> SType s
funT targ tres =
    SType . TyAST
    $ TyAp (SType . TyAST . TyAp (SType $ TyAST tyFunc) $ targ) tres

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

data EVarName = EVarName String
    deriving (Eq, Ord, Show)

data Lit
    = LitNum Double
    | LitString String
    | LitBool Bool
    deriving (Eq, Ord, Show)

data Expr a
    = ELit a Lit
    | EVar a EVarName
    | ELam a EVarName (Expr a)
    | EApp a (Expr a) (Expr a)
    | ELet a EVarName (Expr a) (Expr a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- type FExpr = Fix Expr
--     deriving (Eq, Ord, Show)

----------------------------------------------------------------------
tcon :: String -> Kind -> SType s
tcon name k = SType (TyAST (TyCon (TCon (Id name) k)))

inferLit :: Lit -> SType s
inferLit LitNum{} = tcon "Number" Star
inferLit LitString{} = tcon "String" Star
inferLit LitBool{} = tcon "Bool" Star

data InferState s
    = InferState
      { isContext :: Map EVarName (STRef s (Maybe (SType s)))
      }

type Infer s = StateT (InferState s) (ST s)

lift :: ST s a -> Infer s a
lift act = StateT (\s -> (,s) <$> act)

fresh :: Infer s (STRef s (Maybe a))
fresh = lift $ newSTRef Nothing

infer :: Expr a -> Infer s (Expr (a, SType s), SType s)

infer (ELit a lit) = return (ELit (a, t) lit, t)
    where t = inferLit lit

infer (ELam a var expr) = do
    varRef <- fresh
    is <- get
    let varT = SType $ TyVar varRef
        newContext = Map.insert var varRef (isContext is)
    (expr', exprT) <- lift $ evalStateT (infer expr) (is { isContext = newContext })
    let t = funT varT exprT
    return $ (ELam (a, t) var expr', t)

infer (EVar a var) = do
    is <- get
    case Map.lookup var (isContext is) of
        Nothing -> error $ "bad var " ++ (show var)
        Just ref -> return (EVar (a, t) var, t)
            where t = SType $ TyVar ref

infer (EApp a efun earg) = do
    (efun', efunT) <- infer efun
    (earg', eargT) <- infer earg
    resT <- SType . TyVar <$> fresh
    lift $ unify efunT (funT eargT resT)
    return (EApp (a, resT) efun' earg', resT)

infer (ELet a var edef expr) = do
    varRef <- fresh
    is <- get
    let varT = SType $ TyVar varRef
        newContext = Map.insert var varRef (isContext is)
    (edef', edefT) <- lift $ evalStateT (infer edef) (is { isContext = newContext })
    lift $ unify varT edefT
    (expr', exprT) <- lift $ evalStateT (infer expr) (is { isContext = newContext })
    -- TODO: Generalize varRef
    return (ELet (a, exprT) var edef' expr', exprT)

runInfer :: (forall s. Infer s a) -> a
runInfer act =
    runST $ evalStateT act (InferState { isContext = Map.empty })

inferExpr :: Expr a -> Expr (Maybe Type)
inferExpr expr = runInfer $ do
    (expr', _t) <- infer expr
    lift $ traverse (resolve . snd) expr'

-- Example:

exampleNumber :: Expr (Maybe Type)
exampleNumber = inferExpr (EApp () (ELam () (EVarName "x") (EVar () (EVarName "x"))) (ELit () (LitNum 2)))

exampleLet :: Expr (Maybe Type)
exampleLet = inferExpr (ELet () (EVarName "id") (ELam () (EVarName "x") (EVar () (EVarName "x"))) (EVar () (EVarName "id")))
