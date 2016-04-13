{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fresh.Type where

import           Fresh.Kind (Kind(..))
import qualified Fresh.Kind as Kind
import Data.STRef
import Control.Monad (when, forM_, forM, join, foldM)
import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Foldable

-- import Debug.Trace (traceM)

data Level = Level Int | LevelAny
    deriving (Eq, Show)

instance Ord Level where
    (Level x) `compare` (Level y) = x `compare` y
    l `compare` LevelAny = LT
    LevelAny `compare` l = GT

mapLevel :: (Int -> Int) -> Level -> Level
mapLevel  f (Level x) = Level (f x)
mapLevel _f l         = l

levelInc :: Level -> Level
levelInc = mapLevel (+1)
levelDec :: Level -> Level
levelDec = mapLevel (\x -> x - 1) -- TODO assert > 0

data Id = Id String
    deriving (Eq, Ord, Show)

data TCon = TCon { tcId ::  Id, tcKind :: Kind }
    deriving (Eq, Ord, Show)

data GenVar
    = GenVar
      { genVarId :: Int
      , genVarKind :: Kind
      , genVarLevel :: Level
      }
    deriving (Eq, Ord, Show)

newtype CompositeLabelName = CompositeLabelName String
    deriving (Eq, Ord, Show)

data Composite t
    = CompositeLabel CompositeLabelName t (Composite t)
    | CompositeTerminal
    | CompositeRemainder t
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data FlatComposite t
    = FlatComposite { fcLabels :: (Map CompositeLabelName t)
                    , fcRemainder :: Maybe t }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

flattenComposite :: Composite t -> FlatComposite t
flattenComposite CompositeTerminal = FlatComposite Map.empty Nothing
flattenComposite (CompositeRemainder t) = FlatComposite Map.empty $ Just t
flattenComposite (CompositeLabel n t c) = FlatComposite (Map.insert n t m) end
    where
        (FlatComposite m end) = flattenComposite c

unflattenComposite :: FlatComposite t -> Composite t
unflattenComposite (FlatComposite m mRem) =
    foldr (\(n, t) rest -> CompositeLabel n t rest) rem' $ Map.toList m
    where rem' = maybe CompositeTerminal CompositeRemainder mRem

data TypeAST t
    = TyAp { _tyApFun :: t, _tyApArg :: t }
    | TyCon { _tyCon :: TCon }
    | TyGenVar { _tyGenVar :: GenVar }
    | TyGen { _tyGenVars :: GenVar, _tyGenScheme :: t }
    | TyComp { _tyComp :: Composite t }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class Monad m => HasGen m t where
    freeGenVars :: t -> m (Set GenVar)


instance HasGen m t => HasGen m (TypeAST t) where
    freeGenVars (TyGenVar g) = pure $ Set.singleton g
    freeGenVars (TyGen gv t) = Set.difference <$> freeGenVars t <*> pure (Set.singleton gv)
    freeGenVars t = foldr Set.union Set.empty <$> traverse freeGenVars t

class HasLevel t where
    level :: t -> Level

instance HasLevel GenVar where
    level = genVarLevel
instance HasLevel t => HasLevel (TypeAST t) where
    level = foldr (min . level) LevelAny

class HasKind t where
    kind :: t -> Maybe Kind -- Should really be just Kind, but hard to generate arbitrary for TyAp

instance HasKind TCon where
    kind = Just . tcKind
instance HasKind GenVar where
    kind = Just . genVarKind
instance HasKind t => HasKind (TypeAST t) where
    kind (TyAp f x) = join $ Kind.app <$> kind f <*> kind x
    kind (TyCon tc) = kind tc
    kind (TyGenVar gv) = kind gv
    kind (TyGen v s) = kind s
    kind (TyComp fs) = Just Composite

tyRec :: TypeAST t
tyRec = TyCon (TCon (Id "Rec") (KArrow Composite Star))

tySum :: TypeAST t
tySum = TyCon (TCon (Id "Sum") (KArrow Composite Star))

tyFunc :: TypeAST t
tyFunc = TyCon (TCon (Id "->") (KArrow Star (KArrow Star Star)))

data TVarLink t
    = Unbound Int Level
    | Link t
    deriving (Eq, Ord, Show, Functor)

data TypeVar v t
    = TypeVar { tyVarCell :: v (TVarLink t), tyVarKind :: Kind }
    deriving (Functor)

instance Show (STRef s t) where
    show v = "<stref>"

instance HasGen m t => HasGen m (TVarLink t) where
    freeGenVars (Link t)  = freeGenVars t
    freeGenVars Unbound{} = pure Set.empty

instance HasGen (ST s) t => HasGen (ST s) (TypeVar (STRef s) t) where
    freeGenVars (TypeVar cell _) =
        readSTRef cell >>= freeGenVars

instance HasKind (TypeVar v t) where
    kind (TypeVar c k) = Just k

-- deriving instance Eq t => Eq (TypeVar Identity t)
-- deriving instance Show t => Show (TypeVar Identity t)
deriving instance Eq t => Eq (TypeVar (STRef s) t)
deriving instance Show t => Show (TypeVar (STRef s) t)

data TypeABT v t
    = TyVar (TypeVar v t)
    | TyAST (TypeAST t)
    deriving (Functor)

deriving instance Eq t => Eq (TypeABT (STRef s) t)
deriving instance Show t => Show (TypeABT (STRef s) t)

instance (HasKind t) => HasKind (TypeABT v t) where
    kind (TyVar tv) = kind tv
    kind (TyAST ast) = kind ast

instance HasGen (ST s) t => HasGen (ST s) (TypeABT (STRef s) t) where
    freeGenVars (TyVar tv)  = freeGenVars tv
    freeGenVars (TyAST ast) = freeGenVars ast

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)

instance HasKind (f (Fix f)) => HasKind (Fix f) where
    kind (Fix t) = kind t

instance HasGen m (f (Fix f)) => HasGen m (Fix f) where
    freeGenVars (Fix t) = freeGenVars t

data SType s = SType (TypeABT (STRef s) (SType s))

deriving instance Show (TypeABT (STRef s) (SType s)) => Show (SType s)

instance HasKind (SType s) where
    kind (SType t) = kind t

instance HasGen (ST s) (SType s) where
    freeGenVars (SType t) = freeGenVars t

data Class = Class Id Kind
    deriving (Eq, Ord, Show)

data Pred t = PredIs Class t | PredNoLabel CompositeLabelName t
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data QualType t = QualType { qualPred :: [Pred t], qualType :: t }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasKind t => HasKind (QualType t) where
    kind (QualType _ t) = kind t

emptyQual :: t -> QualType t
emptyQual t = QualType [] t

type QType s = QualType (SType s)

type Type = Fix TypeAST

instance Eq (Fix TypeAST) where
    (Fix x) == (Fix y) = x == y

deriving instance Ord (Fix TypeAST)

funT :: SType s -> SType s -> SType s
funT targ tres =
    SType . TyAST
    $ TyAp (SType . TyAST . TyAp (SType $ TyAST tyFunc) $ targ) tres

recT :: [(CompositeLabelName, SType s)] -> Maybe (SType s) -> SType s
recT fs rest =
    SType . TyAST
    $ TyAp (SType $ TyAST tyRec) (SType . TyAST $ TyComp $ tcomp)
    where
        tcomp = unflattenComposite (FlatComposite (Map.fromList fs) rest)

liftST :: ST s a -> Infer s a
liftST = lift . lift

readVar :: TypeVar (STRef s) t -> Infer s (TVarLink t)
readVar (TypeVar ref k) = liftST $ readSTRef ref

writeVar :: TypeVar (STRef s) t -> TVarLink t -> Infer s ()
writeVar (TypeVar ref k) link = liftST $ writeSTRef ref link

resolve :: SType s -> Infer s (Maybe Type)
resolve (SType (TyVar tvar)) = do
    link <- readVar tvar
    case link of
        Unbound _name level -> throwError
            $ EscapedSkolemError $ "resolve" ++ show tvar ++ ", level: " ++ show level -- TODO perhaps generalize?
        Link t' -> resolve t'
resolve (SType (TyAST (TyGen gv t))) = do
    inLevel $ do
        t' <- resolve t
        return $ Fix . TyGen gv <$> t'
resolve (SType (TyAST (TyGenVar gv))) = do
    curGenLevel <- levelDec <$> getCurrentLevel
    -- if genVarLevel gv < curGenLevel
    --     then throwError $ EscapedSkolemError
    --          $ "genVar : " ++ (show $ genVarLevel gv) ++ " in generalize of " ++ show curGenLevel
    --     else
    return $ Just $ Fix (TyGenVar gv)
resolve (SType (TyAST t)) = do
    mt <- traverse resolve t
    return . fmap Fix $ sequenceA mt

unresolve :: Type -> SType s
unresolve (Fix t) = SType . TyAST $ fmap unresolve t

checkKind :: Maybe Kind -> Infer s Kind
checkKind Nothing  = throwError InvalidKind
checkKind (Just k) = return k

getKind :: HasKind t => t -> Infer s Kind
getKind = checkKind . kind

varBind :: TypeVar (STRef s) (SType s) -> SType s -> Infer s ()
varBind tvar t = do
    tvarK <- getKind tvar
    tK <- getKind t
    when (tvarK /= tK) $ throwError $ KindMismatchError tvarK tK
    vt <- readVar tvar
    case vt of
        Unbound _name l -> writeVar tvar (Link t)
        Link t' -> unify t' t -- TODO occurs

unchain :: SType s -> Infer s (SType s)
unchain t@(SType (TyVar tvar)) = do
    vt <- readVar tvar
    case vt of
        Unbound{} -> return t
        Link t' -> unchain t'
unchain t = return t

unify :: SType s -> SType s -> Infer s ()
unify t1 t2 = do
    k1 <- getKind t1
    k2 <- getKind t2
    when (k1 /= k2) $ throwError $ KindMismatchError k1 k2
    t1' <- unchain t1
    t2' <- unchain t2
    unify' t1' t2'

unify' :: SType s -> SType s -> Infer s ()
unify' (SType (TyVar tvar1)) t2@(SType (TyVar tvar2)) = do
    vt1 <- readVar tvar1
    vt2 <- readVar tvar2
    case (vt1, vt2) of
        (Unbound _n1 l1, Unbound _n2 l2) ->
            if l1 < l2
            then writeVar tvar2 vt1
            else writeVar tvar1 vt2
        _ -> varBind tvar1 t2

unify' (SType (TyVar tvar)) t = varBind tvar t
unify' t (SType (TyVar tvar)) = varBind tvar t
unify' (SType (TyAST t1)) (SType (TyAST t2)) = unifyAST t1 t2

unifyAST :: TypeAST (SType s) -> TypeAST (SType s) -> Infer s ()
unifyAST (TyAp t1 t2) (TyAp t1' t2') = do
    unify t1 t1'
    unify t2 t2'
unifyAST (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
unifyAST (TyGenVar g1) (TyGenVar g2) | g1 == g2 = return ()
unifyAST u1@(TyGen v1 t1) u2@(TyGen v2 t2) = do
    k1 <- getKind v1
    k2 <- getKind v2
    when (k1 /= k2) $ throwError $ KindMismatchError k1 k2
    skolem <- GenVar <$> freshName <*> pure k1 <*> getCurrentLevel
    let t1' = substGen (skolem) t1
        t2' = substGen (skolem) t2
    unify t1' t2'
    gvs1 <- liftST $ freeGenVars u1
    gvs2 <- liftST $ freeGenVars u2
    when (Set.member skolem (gvs1 `Set.union` gvs2) )
        $ throwError
        $ EscapedSkolemError
        $ concat
        [ "Type not polymorphic enough to unify"
        , "\n\t", "Type 1: ", show u1
        , "\n\t", "Type 2: ", show u2
        ]

unifyAST (TyComp c1) (TyComp c2) = do
    let FlatComposite labels1 mEnd1 = flattenComposite c1
        FlatComposite labels2 mEnd2 = flattenComposite c2
        common = Map.intersectionWith (,) labels1 labels2
        in1only = Map.difference labels1 labels2
        in2only = Map.difference labels2 labels1
    -- TODO: wrap errors to say which field failed
    forM_ (Map.elems common) $ uncurry unify
    remainderVar <- freshRVar
    let remainderVarT = SType $ TyVar remainderVar
        fromEnd = TyComp . unflattenComposite . FlatComposite Map.empty . Just
        unifyRemainder rem mEnd =
            if Map.null rem
            then case mEnd of
                Nothing -> return ()
                Just t -> throwError UnificationError -- TODO really?
            else case mEnd of
                Nothing -> throwError UnificationError
                Just end -> unifyAST (TyComp $ unflattenComposite $ FlatComposite rem $ Just remainderVarT) $ fromEnd end
    unifyRemainder in1only mEnd2
    unifyRemainder in2only mEnd1

unifyAST t1 t2 = throwError UnificationError --t1 t2

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
    | EAsc a (QualType Type) (Expr a)
    | EGetField a (Expr a) CompositeLabelName
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


getAnnotation :: Expr a -> a
getAnnotation = head . Data.Foldable.toList

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
      { isContext :: Map EVarName (TypeVar (STRef s) (SType s))
      , isGenFresh :: Int
      , isLevel :: Level
      }
    deriving Show

data TypeError
    = UnificationError --String String
    | EscapedSkolemError String
    | InvalidKind
    | KindMismatchError Kind Kind
    | InvalidVarError String
    deriving (Eq, Show)

type Infer s a = StateT (InferState s) (EitherT TypeError (ST s)) a

getCurrentLevel :: Infer s Level
getCurrentLevel = isLevel <$> get

enterLevel :: Infer s ()
enterLevel = modify (\is -> is { isLevel = levelInc $ isLevel is })

leaveLevel :: Infer s ()
leaveLevel = modify (\is -> is { isLevel = levelDec $ isLevel is })

inLevel :: Infer s a -> Infer s a
inLevel act = do
    enterLevel
    res <- act
    leaveLevel
    return res

listUnion :: Ord a => [a] -> [a] -> [a]
[] `listUnion` y = y
x `listUnion` [] = x
x `listUnion` y = Set.toList $ Set.fromList x `Set.union` Set.fromList y

freshName :: Infer s Int
freshName = do
    is <- get
    let genId = isGenFresh is
    put $ is { isGenFresh = genId + 1 }
    return genId

-- TODO should return a set
getUnbound :: Level -> SType s -> Infer s [TypeVar (STRef s) (SType s)]
getUnbound curLevel (SType (TyVar tv)) = do
    v <- readVar tv
    case v of
        Unbound _ level -> if curLevel < level
                           then pure [tv]
                           else pure []
        Link t' -> getUnbound curLevel t'
getUnbound curLevel (SType (TyAST t)) =
    foldr (++) [] <$> traverse (getUnbound curLevel) t

generalize :: SType s -> Infer s (SType s)
generalize t = do
    curLevel <- getCurrentLevel
    unboundTVars <- getUnbound curLevel t
    let wrapGen t' tv@(TypeVar _ k) = do
            curVar <- readVar tv
            case curVar of
                Link{} -> return t' -- may have been already overwritten (our unboundTVars list isn't unique :()
                Unbound{} -> do
                    gv <- GenVar <$> freshName <*> pure k <*> pure curLevel
                    writeVar tv (Link $ SType $ TyAST $ TyGenVar gv )
                    return $ SType $ TyAST $ TyGen gv t'
    foldM wrapGen t unboundTVars

instantiate :: SType s -> Infer s (SType s)
instantiate (SType (TyAST (TyGen gv t))) = do
    -- TODO

fresh :: Infer s (STRef s (TVarLink t))
fresh = do
    curLevel <- getCurrentLevel
    name <- freshName
    lift . lift $ newSTRef $ Unbound name curLevel

freshTVarK :: Kind -> Infer s (TypeVar (STRef s) a)
freshTVarK k = do
    ref <- fresh
    return $ TypeVar ref k

freshTVar = freshTVarK Star
freshRVar = freshTVarK Composite

subInfer :: InferState s -> Infer s a -> Infer s a
subInfer state' act = do
    res <- lift . lift $ runEitherT $ runStateT act state'
    case res of
        Left err -> throwError err
        Right (x, is') -> do
            modify $ \is -> is { isGenFresh = isGenFresh is' }
            return x

infer :: Show a => Expr a -> Infer s (Expr (a, QType s), QType s)

infer (ELit a lit) = return (ELit (a, t) lit, t)
    where t = emptyQual $ inferLit lit

infer (ELam a var expr) = do
    tvar <- freshTVar
    is <- get
    let varT = SType $ TyVar tvar
        newContext = Map.insert var tvar (isContext is)
    (expr', QualType ps exprT) <- subInfer (is { isContext = newContext }) $ infer expr
    -- exprT' <- instantiate exprT
    let t = QualType ps $ funT varT exprT
    return (ELam (a, t) var expr', t)

infer (EVar a var) = do
    is <- get
    case Map.lookup var (isContext is) of
        Nothing -> throwError $ InvalidVarError (show var)
        Just ref -> return (EVar (a, t) var, t)
            where t = emptyQual $ SType $ TyVar ref

infer (EApp a efun earg) = do
    (efun', QualType efunP efunT) <- infer efun
    (earg', QualType eargP eargT) <- infer earg
    tvar <- freshTVar
    let resT = SType $ TyVar tvar
    unify efunT (funT eargT resT)
    let resQ = QualType (efunP ++ eargP) resT
    return (EApp (a, resQ) efun' earg', resQ)

infer (ELet a var edef expr) = do
    (edef', edefP, edefT) <- inLevel $ do
        tvar <- freshTVar
        is <- get
        (edef', QualType edefP edefT) <- subInfer (is { isContext = Map.insert var tvar (isContext is) }) (infer edef)
        let varT = SType $ TyVar tvar
        unify varT edefT
        return (edef', edefP, edefT)

    genVarT <- generalize edefT
    tvarGen <- freshTVar
    varBind tvarGen genVarT
    is' <- get
    (expr', QualType exprP exprT) <- subInfer (is' { isContext = Map.insert var tvarGen (isContext is') }) (infer expr)
    let resT = QualType (exprP ++ edefP) exprT
    return (ELet (a, resT) var edef' expr', resT)

infer (EAsc a asc@(QualType ps t) expr) = do
    let st = unresolve t
        sps = map (fmap unresolve) ps
    (expr', QualType exprP exprT) <- infer expr
    unify exprT st
    let resT = QualType (sps ++ exprP) exprT
    return (EAsc (a, resT) asc expr', resT)

infer (EGetField a expr name) = do
    (expr', QualType exprP exprT) <- infer expr
    tvar <- SType . TyVar <$> freshTVar
    rvar <- SType . TyVar <$> freshRVar
    unify exprT (recT [(name, tvar)] $ Just rvar)
    let resT = QualType exprP tvar
    return (EGetField (a, resT) expr' name, resT)

runInfer :: (forall s. Infer s a) -> Either TypeError a
runInfer act =
    runST $ runEitherT $ evalStateT act InferState { isContext = Map.empty
                                                    , isGenFresh = 0
                                                    , isLevel = Level 0 }

qresolve :: QType s -> Infer s (QualType Type)
qresolve (QualType ps t) = do
    mt' <- resolve t
    pms' <- traverse (traverse resolve) ps
    let mps' = sequenceA $ map sequenceA pms'
    case (mps', mt') of
        (Just ps', Just t') -> return $ QualType ps' t'
        _ -> throwError $ EscapedSkolemError $ "qresolve:" ++ show mps' ++ " - " ++ show mt'

inferExpr :: Show a => Expr a -> Either TypeError (Expr (QualType Type))
inferExpr expr = runInfer $ do
    (expr', t) <- infer expr
    k <- getKind t
    when (k /= Star) $ throwError $ KindMismatchError k Star
    traverse (qresolve . snd) expr'

