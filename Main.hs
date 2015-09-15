{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, StandaloneDeriving, UndecidableInstances, RecordWildCards #-}
module Main where

import Control.Applicative ((<$>), (<*>), pure, Applicative)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Data.STRef
import Control.Monad.ST


data FakeCell x = FakeCell x
                deriving (Eq, Show)
type Cell s a = STRef s a
type VarName = String

data Expr
    = Var VarName
    | App Expr Expr
    | Lam VarName Expr
    | Let VarName Expr Expr
    deriving (Show)

type TVName = Int
type QName = TVName
type Level = Int

data CTV t
    = Unbound TVName Level
    | Link t
    deriving (Show, Eq)

data CType c t
    = TVar (c (CTV t))
    | QVar QName
    | TArrow t t
--    deriving (Eq)
--instance (Eq t, Eq (c (CTV t))) => Eq (CType c t)

-- fmapCell
--     :: (Applicative f, Functor f)
--     => (a (CTV t) -> f (b (CTV u))) -> CType a t -> f (CType b u)
fmapCell f (TVar c) = TVar <$> f c
fmapCell _ (QVar q) = pure $ QVar q
fmapCell f (TArrow (Fix t1) (Fix t2)) =
    TArrow
    <*> (Fix <$> fmapCell f t1)
    <$> (Fix <$> fmapCell f t2)

data Fix f = Fix { unFix :: f (Fix f) }
deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

type Type s = Fix (CType (STRef s))
type TV s = CTV (Type s)

type PureType = Fix (CType FakeCell)

deriving instance Eq t => Eq (CType FakeCell t)
deriving instance Show t => Show (CType FakeCell t)
deriving instance Eq t => Eq (CType (STRef s) t)
    -- (TVar c1) == (TVar c2) = c1 == c2
    -- (QVar n1) == (QVar n2) = n1 == n2
    -- (TArrow t1 t2) == (TArrow u1 u2) = (t1 == u1) && (t2 == u2)

toFake :: Type s -> ST s PureType
toFake (Fix (TVar stTV)) = do
    val <- readSTRef stTV
    res <-
        case val of
        Unbound n l -> return $ Unbound n l
        Link t -> Link <$> toFake t

    return . Fix . TVar $ FakeCell res
toFake (Fix (QVar q)) = return $ Fix $ QVar q
toFake (Fix (TArrow t1 t2)) = do
    t1' <- toFake t1
    t2' <- toFake t2
    return . Fix $ TArrow t1' t2'

runType :: (forall s. ST s (Type s)) -> PureType
runType f = runST $ do
    t <- f
    toFake t


data Env s =
    Env
    { freshCounter :: Cell s TVName
    , level :: Cell s Level
    }


envEmpty :: ST s (Env s)
envEmpty = do
    fc <- newSTRef 0
    l <- newSTRef 0
    return Env { freshCounter = fc, level = l }

data TypeError
    = OccursError TVName -- Type
    | UnboundVar VarName
    deriving (Show, Eq)

type Infer s a = EitherT TypeError (ReaderT (Env s) (ST s)) a

newCell :: a -> Infer s (Cell s a)
newCell = lift . lift . newSTRef
readCell :: Cell s a -> Infer s a
readCell = lift . lift . readSTRef
writeCell :: Cell s a -> a -> Infer s ()
writeCell c x = lift . lift $ writeSTRef c x
modifyCell :: Cell s a -> (a -> a) -> Infer s a
modifyCell c f = lift . lift $ (modifySTRef c f >> readSTRef c)

enterLevel :: Infer s ()
enterLevel = do
    env <- lift ask
    lift . lift $ modifySTRef (level env) (+1)
    return ()

leaveLevel :: Infer s ()
leaveLevel = do
    env <- lift ask
    lift . lift $ modifySTRef (level env) ((-) 1)
    return ()

currentLevel :: Infer s Level
currentLevel = do
    env <- lift ask
    lift . lift $ readSTRef $ level env

runInfer :: (forall s. Infer s (Type s)) -> Either TypeError PureType
runInfer x = runST $ do
    env <- envEmpty
    t <- runReaderT (runEitherT x) env
    case t of
        Left e -> return $ Left e
        Right st -> Right <$> toFake st

typeError :: TypeError -> Infer s a
typeError = left

fresh :: Infer s TVName
fresh = do
    fc <- freshCounter <$> lift ask
    modifyCell fc (+1)

unify :: Type s -> Type s -> Infer s ()
unify t1 t2
    | t1 == t2  = return ()
    | otherwise = unifyNeq t1 t2


unifyNeq :: Type s -> Type s -> Infer s ()
unifyNeq (Fix (TVar rtv1)) t2 = unifyTVar rtv1 t2
unifyNeq t1 (Fix (TVar rtv2)) = unifyTVar rtv2 t1
unifyNeq (Fix (TArrow a1 b1)) (Fix (TArrow a2 b2)) = do
    unify a1 a2
    unify b1 b2
unifyNeq (Fix (QVar _)) _ = error "Escaped qvar"
unifyNeq _ (Fix (QVar _)) = error "Escaped qvar"

unifyTVar :: Cell s (TV s) -> Type s -> Infer s ()
unifyTVar ioTV t2 = do
    tv <- readCell ioTV
    case tv of
        Unbound name level -> do
            occurs name ioTV t2
            writeCell ioTV $ Link t2
        Link t1 -> unify t1 t2

occurs :: TVName -> Cell s (TV s) -> Type s -> Infer s ()
occurs name ioTV (Fix (TVar ioTV2))
    | ioTV == ioTV2 = typeError $ OccursError name
    | otherwise = do
          tv2 <- readCell ioTV2
          case tv2 of
              (Unbound name l2) -> do
                  tv <- readCell ioTV
                  case tv of
                      Unbound _ l1 ->
                          writeCell ioTV $ Unbound name $ min l1 l2
                      _ -> return ()
              Link t2 -> occurs name ioTV t2
occurs name tv (Fix (TArrow t2 t3)) =
    let check = occurs name tv
    in check t2 >> check t3
occurs _ _ (Fix (QVar _)) = return ()


gen :: Type s -> Infer s (Type s)
gen t@(Fix (TVar ioTV)) = do
    tv <- readCell ioTV
    case tv of
        Unbound name level -> do
            curLevel <- currentLevel
            return $ if curLevel < level
                     then Fix $ QVar name
                     else t
        Link t -> gen t
gen (Fix (TArrow ta tb)) = do
    gta <- gen ta
    gtb <- gen tb
    return $ Fix $ TArrow gta gtb
gen t = return t

----------------------------------------------------------------------

newTVar :: Infer s (Type s)
newTVar = do
    tvName <- fresh
    level <- currentLevel
    Fix . TVar <$> newCell (Unbound tvName level)

type TEnv s = [(QName, Type s)]

tenvEmpty :: TEnv s
tenvEmpty = []

inst' :: TEnv s -> Type s -> Infer s (Type s, TEnv s)
inst' env (Fix (QVar name)) =
    case lookup name env of
    Just t -> return (t, env)
    Nothing -> do
        tv <- newTVar
        return (tv, (name, tv) : env)
inst' env t@(Fix (TVar ioTV)) = do
    tv <- readCell ioTV
    case tv of
        Unbound _ _ -> return (t, env)
        Link tLink -> inst' env tLink
inst' env (Fix (TArrow ta tb)) = do
    (ta', envA) <- inst' env ta
    (tb', envB) <- inst' envA tb
    return (Fix $ TArrow ta' tb', envB)

inst :: Type s -> Infer s (Type s)
inst t = fst <$> inst' tenvEmpty t

type VEnv s = [(VarName, Type s)]

typeOf :: VEnv s -> Expr -> Infer s (Type s)
typeOf env (Var name) =
    case lookup name env of
    Just t -> inst t
    Nothing -> typeError $ UnboundVar name
typeOf env (App e1 e2) = do
    tFun <- typeOf env e1
    tArg <- typeOf env e2
    tRes <- newTVar
    unify tFun $ Fix $ TArrow tArg tRes
    return tRes
typeOf env (Lam name e) = do
    tArg <- newTVar
    tBody <- typeOf ((name,tArg) : env) e
    return $ Fix $ TArrow tArg tBody
typeOf env (Let name e1 e2) = do
    enterLevel
    tDef <- typeOf env e1 -- non-recursive let
    leaveLevel
    tVar <- gen tDef
    typeOf ((name,tVar) : env) e2


--infer :: Expr -> Either TypeError PureType
--infer = runInfer . typeOf []

test_id_inner = (Lam "x" $ Var "x")
test_id = Let "id" test_id_inner (Var "id")
tid = runInfer $ typeOf [] test_id

test_id2 = (Lam "x" (Let "y" (Var "x") (Var "y")))
tid2 = runInfer $ typeOf [] test_id2

test_id3 = Let "id" (Lam "y" $ (Lam "x" $ Var "y")) (Var "id")
tid3 = runInfer $ typeOf [] test_id3


main = do
    print tid
    print tid2
    print tid3

