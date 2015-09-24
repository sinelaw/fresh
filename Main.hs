{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, StandaloneDeriving, UndecidableInstances, RecordWildCards #-}
module Main where

import Control.Monad (foldM)
import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.STRef

class Render a where
    render :: a -> String

data FakeCell x = FakeCell x
                deriving (Eq, Show)

instance Render x => Render (FakeCell x) where
    render (FakeCell x) = render x

type Cell s a = STRef s a
type VarName = String

data Expr
    = Var VarName
    | App Expr Expr
    | Lam VarName Expr
    | Let VarName Expr Expr
    | Pair Expr Expr
    deriving (Show)

instance Render Expr where
    render (Var name) = name
    render (App e1 e2) = render e1 ++ " " ++ render e2
    render (Lam name e) = "(\\" ++ name ++ " -> " ++ render e ++ ")"
    render (Let name e1 e2) = "let " ++ name ++ " = " ++ render e1 ++ " in " ++ render e2
    render (Pair e1 e2) = "(" ++ render e1 ++ ", " ++ render e2 ++ ")"

instance Render Int where
    render = show

instance Render String where
    render = show

type TVName = Int
type QName = TVName
type Level = Int

genericLevel :: Int
genericLevel = maxBound

markedLevel :: Int
markedLevel = -1

data Levels a =
    Levels
    { levelOld :: a
    , levelNew :: a
    }
    deriving (Show, Eq)

fmapLevelCell :: Applicative f => (a -> f b) -> Levels a -> f (Levels b)
fmapLevelCell f (Levels old new) = Levels <$> (f old) <*> (f new)

data CTV t
    = Unbound TVName Level
    | Link t
    deriving (Show, Eq)

instance (Render t) => Render (CTV t) where
    render (Unbound tv l) = "t" ++ show tv  -- ++ "-" ++ show l
    render (Link t) = render t

data CType c t
    = TVar (c (CTV t))
    | TArrow t t (Levels (c Level))
    | TPair t t (Levels (c Level))

fmapCell ::
    Applicative f =>
    (c (CTV (Fix (CType c))) -> f (c' (CTV (Fix (CType c')))))
    -> (c Level -> f (c' Level))
    -> Fix (CType c) -> f (Fix (CType c'))
fmapCell f _ (Fix (TVar c)) = Fix . TVar <$> f c
fmapCell f g (Fix (TArrow t1 t2 level)) =
    fmap Fix $ TArrow <$> (fmapCell f g t1) <*> (fmapCell f g t2) <*> (fmapLevelCell g level)
fmapCell f g (Fix (TPair t1 t2 level)) =
    fmap Fix $ TPair <$> (fmapCell f g t1) <*> (fmapCell f g t2) <*> (fmapLevelCell g level)

instance (Render (c (CTV t)), Render t) => Render (CType c t) where
    render (TVar c) = render c
    render (TArrow t1 t2 _l) = render t1 ++ " -> " ++ render t2
    render (TPair t1 t2 _l) = "(" ++ render t1 ++ ", " ++ render t2 ++ ")"

data Fix f = Fix { unFix :: f (Fix f) }
deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)
instance Render (f (Fix f)) => Render (Fix f) where
    render (Fix x) = render x

type Type s = Fix (CType (STRef s))
type TV s = CTV (Type s)

type PureType = Fix (CType FakeCell)

deriving instance Eq t => Eq (CType FakeCell t)
deriving instance Show t => Show (CType FakeCell t)
deriving instance Eq t => Eq (CType (STRef s) t)

toFake :: Type s -> ST s PureType
toFake t = fmapCell stToFake (\x -> FakeCell <$> readSTRef x) t
    where
        stToFake stTV = do
            val <- readSTRef stTV
            res <-
                case val of
                Unbound n l -> return $ Unbound n l
                Link t' -> Link <$> toFake t'
            return $ FakeCell res

runType :: (forall s. ST s (Type s)) -> PureType
runType f = runST $ do
    t <- f
    toFake t


data Env s =
    Env
    { freshCounter :: Cell s TVName
    , level :: Cell s Level
    , toBeLevelAdjusted :: Cell s [Type s]
    }


envEmpty :: ST s (Env s)
envEmpty = do
    fc <- newSTRef 0
    l <- newSTRef 0
    tbla <- newSTRef []
    return
        Env
        { freshCounter = fc
        , level = l
        , toBeLevelAdjusted = tbla
        }

data TypeError t
    = OccursError
    | UnboundVar VarName
    | EscapedGenericLevel
    | UnificationError t t
    deriving (Show, Eq, Functor, Foldable, Traversable)

instance Render t => Render (TypeError t) where
    render = show . fmap render

type Infer s a = EitherT (TypeError (Type s)) (ReaderT (Env s) (ST s)) a

newCell :: a -> Infer s (Cell s a)
newCell = lift . lift . newSTRef
readCell :: Cell s a -> Infer s a
readCell = lift . lift . readSTRef
writeCell :: Cell s a -> a -> Infer s ()
writeCell c x = lift . lift $ writeSTRef c x
modifyCell :: Cell s a -> (a -> a) -> Infer s a
modifyCell c f = lift . lift $ (modifySTRef c f >> readSTRef c)

enqueueAdj :: Type s -> Infer s ()
enqueueAdj t = do
    env <- lift ask
    lift . lift $ modifySTRef (toBeLevelAdjusted env) (t:)
    return ()

enterLevel :: Infer s ()
enterLevel = do
    env <- lift ask
    lift . lift $ modifySTRef (level env) (1 +)
    return ()

leaveLevel :: Infer s ()
leaveLevel = do
    env <- lift ask
    lift . lift $ modifySTRef (level env) (subtract 1)
    return ()

currentLevel :: Infer s Level
currentLevel = do
    env <- lift ask
    lift . lift $ readSTRef $ level env

runInfer :: (forall s. Infer s (Type s)) -> Either (TypeError PureType) PureType
runInfer x = runST $ do
    env <- envEmpty
    t <- runReaderT (runEitherT x) env
    case t of
        Left e -> do
            e' <- traverse toFake e
            return $ Left e'
        Right st -> Right <$> toFake st

instance (Render a, Render b) => Render (Either a b) where
    render (Left x) = "Error: " ++ render x
    render (Right y) = render y

typeError :: TypeError (Type s) -> Infer s a
typeError = left

--cycleFree2 :: Type s -> Type s -> Levels -> Infer s ()
cycleFree2 t1 t2 ls = do
    lNew <- readCell $ levelNew ls
    when (lNew == markedLevel) $ typeError OccursError
    writeCell (levelNew ls) markedLevel
    cycleFree t1
    cycleFree t2
    writeCell (levelNew ls) lNew

cycleFree :: Type s -> Infer s ()
cycleFree (Fix (TVar iotv)) = do
    tv <- readCell iotv
    case tv of
        Unbound{} -> return ()
        Link t -> cycleFree t
cycleFree (Fix (TArrow t1 t2 ls)) = cycleFree2 t1 t2 ls
cycleFree (Fix (TPair t1 t2 ls)) = cycleFree2 t1 t2 ls
cycleFree _ = return ()

repr :: Type s -> Infer s (Type s)
repr t@(Fix (TVar iotv)) = do
    tv <- readCell iotv
    case tv of
        Link t' -> do
            reprT <- repr t'
            writeCell iotv $ Link reprT
            return reprT
        _ -> return t
repr t = return t

getLevel :: Type s -> Infer s Level
getLevel (Fix (TVar iotv)) = do
    tv <- readCell iotv
    case tv of
        Unbound _ l -> return l
        _ -> error "getLevel only works for unbound tvars, and composites"
getLevel (Fix (TArrow _ _ levels)) = readCell $ levelNew levels
getLevel (Fix (TPair _ _ levels)) = readCell $ levelNew levels
getLevel _ = error "getLevel only works for unbound tvars, and composites"

fresh :: Infer s TVName
fresh = do
    fc <- freshCounter <$> lift ask
    modifyCell fc (+1)

unify :: Type s -> Type s -> Infer s ()
unify t1 t2
    | t1 == t2  = return ()
    | otherwise = unifyNeq t1 t2


unifyTConstr ts1 l1 ts2 l2 = do
    lNew1 <- readCell $ levelNew l1
    lNew2 <- readCell $ levelNew l2
    when ((lNew1 == markedLevel) || (lNew2 == markedLevel)) $
        typeError OccursError
    let minLevel = min lNew1 lNew2
    writeCell (levelNew l1) markedLevel
    writeCell (levelNew l2) markedLevel
    mapM_ (uncurry $ unifyLev minLevel) $ zip ts1 ts2
    writeCell (levelNew l1) minLevel
    writeCell (levelNew l2) minLevel


unifyNeq :: Type s -> Type s -> Infer s ()
unifyNeq (Fix (TVar rtv1)) (Fix (TVar rtv2)) = unifyTVars rtv1 rtv2
unifyNeq (Fix (TVar rtv1)) t2 = unifyTVar rtv1 t2
unifyNeq t1 (Fix (TVar rtv2)) = unifyTVar rtv2 t1
unifyNeq (Fix (TArrow a1 b1 l1)) (Fix (TArrow a2 b2 l2)) = unifyTConstr [a1,b1] l1 [a2,b2] l2
unifyNeq (Fix (TPair a1 b1 l1)) (Fix (TPair a2 b2 l2)) = unifyTConstr [a1,b1] l1 [a2,b2] l2
unifyNeq _ _ = error "Unification failed"


unifyTVars :: Cell s (TV s) -> Cell s (TV s) -> Infer s ()
unifyTVars ioTV1 ioTV2 = do
    tv1 <- readCell ioTV1
    tv2 <- readCell ioTV2
    case (tv1, tv2) of
        (Unbound _ l1, Unbound _ l2) ->
            if l1 > l2
                then link ioTV1 ioTV2
                else link ioTV2 ioTV1
            where
                link iotv dest = writeCell iotv . Link . Fix . TVar $ dest
        (Unbound _ l1, _) -> update' l1 ioTV1 ioTV2
        (_, Unbound _ l2) -> update' l2 ioTV2 ioTV1
        (Link t1, Link t2) -> unify t1 t2
    where
        update' l dest updateT = do
            updateLevel l . Fix . TVar $ updateT
            writeCell dest $ Link . Fix . TVar $ updateT

unifyTVar :: Cell s (TV s) -> Type s -> Infer s ()
unifyTVar ioTV t2 = do
    tv <- readCell ioTV
    case tv of
        Unbound _ level -> do
            updateLevel level t2
            writeCell ioTV $ Link t2
        Link t1 -> unify t1 t2

unifyLev :: Level -> Type s -> Type s -> Infer s ()
unifyLev l t1 t2 = do
    t1' <- repr t1
    updateLevel l t1'
    unify t1' t2

updateLevelErrorHack :: Infer s ()
updateLevelErrorHack = error "Update level works only for unbound tvars, and composites"

updateLevel :: Level -> Type s -> Infer s ()
updateLevel l (Fix (TVar iotv)) = do
    tv <- readCell iotv
    case tv of
        Unbound n l' -> do
            when (l' == genericLevel) $ typeError EscapedGenericLevel
            if l < l'
                then writeCell iotv $ Unbound n l
                else return ()
        Link _ -> updateLevelErrorHack
updateLevel l t@(Fix (TArrow _ _ levels)) = do
    lNew <- readCell $ levelNew levels
    when (lNew == genericLevel) $ typeError EscapedGenericLevel
    when (lNew == markedLevel) $ typeError OccursError
    when (l < lNew) $ do
        lOld <- readCell $ levelOld levels
        when (lNew == lOld) $ enqueueAdj t
        writeCell (levelNew levels) l
        return ()

updateLevel _ _ = updateLevelErrorHack

----------------------------------------------------------------------

loop :: [Type s] -> Level -> Type s -> Infer s [Type s]
loop acc level ty = do
    ty' <- repr ty
    case ty' of
        Fix (TVar iotv) -> do
            tv <- readCell iotv
            case tv of
                Unbound name l -> do
                    when (l > level) $
                        writeCell iotv $ Unbound name level
                    return acc
                _ -> return acc
        Fix (TArrow t1 t2 levels) -> do
            lNew <- readCell (levelNew levels)
            when (lNew == markedLevel) $ typeError OccursError
            when (lNew > level) $ writeCell (levelNew levels) level
            adjustOne acc ty'
        _ -> return acc

adjustOne :: [Type s] -> Type s -> Infer s [Type s]
adjustOne acc t@(Fix (TArrow t1 t2 levels)) = do
    lOld <- readCell (levelOld levels)
    lCur <- currentLevel
    if (lOld <= lCur)
        then return $ t : acc
        else do
        lNew <- readCell (levelNew levels)
        if (lNew == lOld)
            then return acc
            else do
            writeCell (levelNew levels) markedLevel
            acc1 <- loop acc lNew t1
            acc2 <- loop acc1 lNew t2
            writeCell (levelNew levels) lNew
            writeCell (levelOld levels) lNew
            return acc2
adjustOne _ _ = error $ "Unexpected type" -- ++ show t

forceDelayedAdj :: Infer s ()
forceDelayedAdj = do
    env <- lift ask
    tbla <- lift . lift $ readSTRef (toBeLevelAdjusted env)
    tbla' <- foldM adjustOne [] tbla
    lift . lift $ writeSTRef (toBeLevelAdjusted env) tbla'

----------------------------------------------------------------------
gen :: Type s -> Infer s ()
gen t = do
    forceDelayedAdj
    loop t
    where
        loop t = repr t >>= gen'
        gen' :: Type s -> Infer s ()
        gen' t@(Fix (TVar ioTV)) = do
            tv <- readCell ioTV
            case tv of
                Unbound name level -> do
                    curLevel <- currentLevel
                    when (level > curLevel) $ do
                        writeCell ioTV $ Unbound name genericLevel
                Link t' -> loop t'
        gen' (Fix (TArrow ta tb ls)) = do
            lNew <- readCell $ levelNew ls
            curLevel <- currentLevel
            when (lNew > curLevel) $ do
                loop ta
                loop tb
                gtaLevel <- getLevel ta
                gtbLevel <- getLevel tb
                let l = max gtaLevel gtbLevel
                writeCell (levelOld ls) l
                writeCell (levelNew ls) l
        gen' _ = return ()

----------------------------------------------------------------------

newTVar :: Infer s (Type s)
newTVar = do
    tvName <- fresh
    level <- currentLevel
    Fix . TVar <$> newCell (Unbound tvName level)

newArrow :: Type s -> Type s -> Infer s (Type s)
newArrow t1 t2 = do
    lOld <- newCell 0
    lNew <- newCell 0
    return . Fix . TArrow t1 t2 $ Levels lOld lNew

type TEnv s = [(QName, Type s)]

tenvEmpty :: TEnv s
tenvEmpty = []

inst' :: TEnv s -> Type s -> Infer s (Type s, TEnv s)
inst' env t@(Fix (TVar ioTV)) = do
    tv <- readCell ioTV
    case tv of
        Unbound n l -> do
            if (l == genericLevel)
                then do
                case lookup n env of
                    Nothing -> do
                        freshTV <- newTVar
                        return (freshTV, (n,freshTV):env)
                    Just t' -> return (t', env)
                else return (t, env)
        Link tLink -> inst' env tLink
inst' env (Fix (TArrow ta tb l)) = do
    (ta', envA) <- inst' env ta
    (tb', envB) <- inst' envA tb
    return (Fix $ TArrow ta' tb' l, envB)
inst' env (Fix (TPair ta tb l)) = do
    (ta', envA) <- inst' env ta
    (tb', envB) <- inst' envA tb
    return (Fix $ TPair ta' tb' l, envB)

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
    tArr <- newArrow tArg tRes
    unify tFun tArr
    return tRes
typeOf env (Lam name e) = do
    tArg <- newTVar
    tBody <- typeOf ((name,tArg) : env) e
    newArrow tArg tBody
typeOf env (Let name e1 e2) = do
    enterLevel
    tDef <- typeOf env e1 -- non-recursive let
    leaveLevel
    gen tDef
    typeOf ((name,tDef) : env) e2
typeOf env (Pair e1 e2) = do
    t1 <- typeOf env e1
    t2 <- typeOf env e2
    old <- newCell 0
    new <- newCell 0
    return $ Fix $ TPair t1 t2 (Levels old new)


--infer :: Expr -> Either TypeError PureType
--infer = runInfer . typeOf []

test_id_inner = (Lam "x" $ Var "x")
test_id = Let "id" test_id_inner (App (Var "id") (Var "id"))
tid = runInfer $ typeOf [] test_id

test_id2 = (Lam "x" (Let "y" (Var "x") (Var "y")))
tid2 = runInfer $ typeOf [] test_id2

test_id3 = Let "id" (Lam "y" $ (Lam "x" $ Var "y")) (Var "id")
tid3 = runInfer $ typeOf [] test_id3


main :: IO ()
main = do
    putStrLn $ render tid
    putStrLn $ render tid2
    putStrLn $ render tid3

