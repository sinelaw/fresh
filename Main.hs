{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, StandaloneDeriving, UndecidableInstances, RecordWildCards #-}
module Main where


import           Control.Unification (Unifiable(..), UTerm(..), BindingMonad(..))
import qualified Control.Unification as Unification
import           Control.Unification.Types (UFailure(..))
import           Control.Unification.STVar (STVar, STBinding, runSTBinding)
import Data.Functor.Fixedpoint (Fix(..))
import qualified Data.Map as Map
import Data.Map (Map)

-- import Control.Monad.Identity
-- import Control.Monad.Trans
-- import Control.Monad.Trans.Error
-- import Data.Foldable
-- import Data.Traversable

-- import Control.Monad (foldM)
-- import Control.Monad (when)
-- import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
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
              deriving (Show, Eq)

data Type t
    = QVar QName
    | TArrow t t
    deriving (Show, Functor, Foldable, Traversable)

instance Unifiable Type where
    zipMatch (QVar x) (QVar y) | x == y = Just $ QVar x
    zipMatch (TArrow l1 l2) (TArrow r1 r2) = Just $ TArrow (Right (l1, r1)) (Right (l2, r2))
    zipMatch _ _ = Nothing

type TTerm s = UTerm Type (STVar s Type)

data Env s =
    Env {
        typeEnv :: Map VarName (TTerm s)
    }

envEmpty = Env { typeEnv = Map.empty }

type InferT s m a = ReaderT (Env s) (EitherT (UFailure Type (STVar s Type)) m) a
type Infer s a = InferT s (STBinding s) a

eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing

runInfer :: Traversable t => (forall s. Infer s (UTerm t v)) -> Maybe (Fix t)
runInfer act = runReaderT envEmpty . eitherToMaybe . runEitherT $ do
    t <- act
    lift . Unification.freeze $ t


getEnv :: Infer s (Env s)
getEnv = ask

withVar :: VarName -> TTerm s -> Infer s a -> Infer s a
withVar n t = local (\env -> env { typeEnv = Map.insert n t $ typeEnv env })

fresh :: Infer s (STVar s Type)
fresh = lift $ lift freeVar

unify :: TTerm s -> TTerm s -> Infer s (TTerm s)
unify x y = lift $ Unification.unify x y

typeOf :: FExpr -> Infer s (TTerm s)
typeOf (FExpr e) = case e of
    Var n -> do
        tenv <- typeEnv <$> getEnv
        case Map.lookup n tenv of
            Nothing -> error $ "Unbound var: " ++ show n
            Just t -> return t
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
test_id = FExpr $ Let "id" test_id_inner (FExpr $ Var "id")
tid = runInfer $ typeOf test_id

test_id2 = FExpr (Lam "x" (FExpr $ Let "y" (FExpr $ Var "x") (FExpr $ Var "y")))
tid2 = runInfer $ typeOf test_id2

test_id3 = FExpr $ Let "id" (FExpr $ Lam "y" $ (FExpr $ Lam "x" (FExpr $ Var "y"))) (FExpr $ Var "id")
tid3 = runInfer $ typeOf test_id3


main :: IO ()
main = --return ()
  do
    print tid
    print tid2
    print tid3

