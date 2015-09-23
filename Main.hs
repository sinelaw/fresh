{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, StandaloneDeriving, UndecidableInstances, RecordWildCards, LambdaCase #-}
module Main (main) where

import Control.Monad ((>=>))
import           Control.Unification (Unifiable(..), UTerm(..), BindingMonad(..))
import qualified Control.Unification as Unification
import           Control.Unification.Types (UFailure(..))
import           Control.Unification.STVar (STVar, STBinding, runSTBinding)
import Data.Functor.Fixedpoint (Fix(..))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- import Control.Monad.Identity
-- import Control.Monad.Trans
-- import Control.Monad.Trans.Error
-- import Data.Foldable
-- import Data.Traversable

-- import Control.Monad (foldM)
-- import Control.Monad (when)
-- import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT, left, bimapEitherT)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.State (evalStateT, get, put)
-- import Data.STRef

type VarName = String

data Expr e
    = Var VarName
    | App e e
    | Lam VarName e
    | Let VarName e e
    deriving (Show, Eq, Functor, Foldable, Traversable)

newtype FExpr = FExpr (Expr FExpr)
              deriving (Show, Eq)

newtype QName = QName String
              deriving (Show, Eq, Ord)

data Type t
    = QVar QName
    | TArrow t t
    deriving (Show, Functor, Foldable, Traversable)

instance Unifiable Type where
    zipMatch (QVar x) (QVar y) | x == y = Just $ QVar x
    zipMatch (TArrow l1 l2) (TArrow r1 r2) = Just $ TArrow (Right (l1, r1)) (Right (l2, r2))
    zipMatch _ _ = Nothing

newtype TVar s = TVar { stVar :: STVar s Type }
               deriving (Show, Eq)

instance Ord (TVar s) where
    x `compare` y = Unification.getVarID (stVar x) `compare` Unification.getVarID (stVar y)

type TTerm s = UTerm Type (STVar s Type)

data Env s =
    Env
    { typeEnv :: Map VarName (TTerm s)
    }

envEmpty :: Env s
envEmpty = Env { typeEnv = Map.empty }

type InferT s m a = ReaderT (Env s) (EitherT (UFailure Type (STVar s Type)) m) a
type Infer s a = InferT s (STBinding s) a

liftUnify :: Monad (m s) => m s a -> InferT s (m s) a
liftUnify = lift . lift

getEnv :: Infer s (Env s)
getEnv = ask

withVar :: VarName -> TTerm s -> Infer s a -> Infer s a
withVar n t = local (\env -> env { typeEnv = Map.insert n t $ typeEnv env })

liveTVars :: Infer s (Set (TVar s))
liveTVars = do
    mappedTypes <- Map.elems . typeEnv <$> ask
    Set.fromList . map TVar . mconcat <$> mapM (liftUnify . Unification.getFreeVars) mappedTypes


gen :: TTerm s -> Infer s () -- TTerm s)
gen t = do
    lives <- liveTVars
    frees <- (`Set.difference` lives) . Set.fromList . fmap TVar <$> (liftUnify $ Unification.getFreeVars t)
    liftUnify $ mapM_ (\(TVar v) -> Unification.bindVar v $ mkQVar v) frees
    where
        mkQVar v = UTerm . QVar . QName . show $ Unification.getVarID v

inst :: TTerm s -> Infer s (TTerm s)
inst ts0 = lift $ evalStateT (loop ts0) Map.empty
    where
    loop = (lift . lift . Unification.semiprune) >=>
        \case
            UTerm t ->
                case t of
                QVar n -> do
                    qvars <- get
                    UVar <$> case Map.lookup n qvars of
                         Nothing -> do
                             qvarT <- lift (lift Unification.freeVar)
                             put $ Map.insert n qvarT qvars
                             return qvarT
                         Just t' -> return t'
                TArrow t1 t2 -> do
                    t1' <- loop t1
                    t2' <- loop t2
                    return . UTerm $ TArrow t1' t2'
            UVar  v -> do
                tv <- lift . lift $ Unification.lookupVar v
                case tv of
                    Nothing -> return $ UVar v
                    Just t' -> loop t'

-- runInfer :: Traversable t => (forall s. Infer s (UTerm t v)) -> Either String (Maybe (Fix t))
runInfer :: (forall s. Infer s (TTerm s)) -> Either String (Maybe (Fix Type))
runInfer act = runSTBinding $ do
    t <- runEitherT . flip runReaderT envEmpty $ do
        withFrees <- act
        gen withFrees -- HACK, so freeze won't return Nothing
        lift $ Unification.applyBindings withFrees
    case t of
        Left e -> return . Left . show $ e
        Right t' -> return . Right $ Unification.freeze t'

fresh :: Infer s (STVar s Type)
fresh = lift $ lift freeVar

unify :: TTerm s -> TTerm s -> Infer s (TTerm s)
unify x y = lift $ Unification.unify x y

typeOf :: FExpr -> Infer s (TTerm s)
typeOf (FExpr e') = case e' of
    Var n -> do
        tenv <- typeEnv <$> getEnv
        case Map.lookup n tenv of
            Nothing -> error $ "Unbound var: " ++ show n
            Just t -> inst t -- lift $ Unification.freshen t
    App eFun eArg -> do
        tFun <- typeOf eFun
        tArg <- typeOf eArg
        tRes <- UVar <$> fresh
        UTerm (TArrow _ tRes') <- unify tFun $ UTerm $ TArrow tArg tRes
        return tRes'
    Lam n e -> do
        tArg <- UVar <$> fresh
        tBody <- withVar n tArg $ typeOf e
        return $ UTerm $ TArrow tArg tBody
    Let n e1 e2 -> do
        --tArg <- UVar <$> fresh
        --tDef <- withVar n tArg $ typeOf e1
        tDef <- typeOf e1
        gen tDef
        withVar n tDef $ typeOf e2
-- ----------------------------------------------------------------------

-- gen :: Type s -> Infer s (Type s)
-- gen t@(Fix (TVar ioTV)) = do
--     tv <- readCell ioTV
--     case tv of
--         Unbound name level -> do
--             curLevel <- currentLevel
--             return $ if curLevel < level
--                      then Fix $ QVar name
--                      else t
--         Link t' -> gen t'
-- gen (Fix (TArrow ta tb l)) = do
--     gta <- gen ta
--     gtb <- gen tb
--     return $ Fix $ TArrow gta gtb l
-- gen t = return t

-- ----------------------------------------------------------------------

-- newTVar :: Infer s (Type s)
-- newTVar = do
--     tvName <- fresh
--     level <- currentLevel
--     Fix . TVar <$> newCell (Unbound tvName level)

-- newArrow :: Type s -> Type s -> Infer s (Type s)
-- newArrow t1 t2 = do
--     lOld <- newCell 0
--     lNew <- newCell 0
--     return . Fix . TArrow t1 t2 $ Levels lOld lNew

-- type TEnv s = [(QName, Type s)]

-- tenvEmpty :: TEnv s
-- tenvEmpty = []

-- inst' :: TEnv s -> Type s -> Infer s (Type s, TEnv s)
-- inst' env (Fix (QVar name)) =
--     case lookup name env of
--     Just t -> return (t, env)
--     Nothing -> do
--         tv <- newTVar
--         return (tv, (name, tv) : env)
-- inst' env t@(Fix (TVar ioTV)) = do
--     tv <- readCell ioTV
--     case tv of
--         Unbound _ _ -> return (t, env)
--         Link tLink -> inst' env tLink
-- inst' env (Fix (TArrow ta tb l)) = do
--     (ta', envA) <- inst' env ta
--     (tb', envB) <- inst' envA tb
--     return (Fix $ TArrow ta' tb' l, envB)

-- inst :: Type s -> Infer s (Type s)
-- inst t = fst <$> inst' tenvEmpty t

-- type VEnv s = [(VarName, Type s)]

-- typeOf :: VEnv s -> Expr -> Infer s (Type s)
-- typeOf env (Var name) =
--     case lookup name env of
--     Just t -> inst t
--     Nothing -> typeError $ UnboundVar name
-- typeOf env (App e1 e2) = do
--     tFun <- typeOf env e1
--     tArg <- typeOf env e2
--     tRes <- newTVar
--     old <- newCell 0
--     new <- newCell 0
--     unify tFun $ Fix $ TArrow tArg tRes (Levels old new) -- hack
--     return tRes
-- typeOf env (Lam name e) = do
--     tArg <- newTVar
--     tBody <- typeOf ((name,tArg) : env) e
--     old <- newCell 0
--     new <- newCell 0
--     return $ Fix $ TArrow tArg tBody (Levels old new) -- hack
-- typeOf env (Let name e1 e2) = do
--     enterLevel
--     tDef <- typeOf env e1 -- non-recursive let
--     leaveLevel
--     tVar <- gen tDef
--     typeOf ((name,tVar) : env) e2


-- --infer :: Expr -> Either TypeError PureType
-- --infer = runInfer . typeOf []

test_id_inner = FExpr (Lam "x" $ FExpr $ Var "x")
test_id = FExpr $ Let "id" test_id_inner (FExpr $ Var "id") -- (FExpr $ App (FExpr $ Var "id") (FExpr $ Var "id")) -- (FExpr $ Var "id")
tid = runInfer $ typeOf test_id

test_id2 = FExpr (Lam "x" (FExpr $ Let "y" (FExpr $ Var "x") (FExpr $ Var "y")))
tid2 = runInfer $ typeOf test_id2

test_id3 = FExpr $ Let "id" (FExpr $ Lam "y" $ (FExpr $ Lam "x" (FExpr $ Var "y"))) (FExpr $ App (FExpr $ Var "id") (FExpr $ Var "id"))
tid3 = runInfer $ typeOf test_id3


main :: IO ()
main = --return ()
  do
    print tid
    print tid2
    print tid3

