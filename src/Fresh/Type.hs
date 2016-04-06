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
import Control.Monad (when, forM_)
import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Set (Set)

import qualified Data.Set as Set
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Foldable

-- import Debug.Trace (traceM)

data Id = Id String
    deriving (Eq, Ord, Show)

data TCon = TCon { tcId ::  Id, tcKind :: Kind }
    deriving (Eq, Ord, Show)

data GenVar = GenVar { genVarId :: Int, genVarKind :: Kind }
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
    | TyGen { _tyGenVars :: [GenVar], _tyGenScheme :: t }
    | TyComp { _tyComp :: Composite t }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class HasKind t where
    kind :: t -> Kind

instance HasKind TCon where
    kind = tcKind
instance HasKind GenVar where
    kind = genVarKind
instance HasKind t => HasKind (TypeAST t) where
    kind (TyAp f x) =
        case Kind.app (kind f) (kind x) of
            Nothing -> error $ "Can't apply type function, kinding mismatch! " ++ show (kind f) ++ " on " ++ show (kind x)
            Just k -> k
    kind (TyCon tc) = kind tc
    kind (TyGenVar gv) = kind gv
    kind (TyGen vs s) = kind s
    kind (TyComp fs) = Composite

tyRec :: TypeAST t
tyRec = TyCon (TCon (Id "Rec") (KArrow Composite Star))

tySum :: TypeAST t
tySum = TyCon (TCon (Id "Sum") (KArrow Composite Star))

tyFunc :: TypeAST t
tyFunc = TyCon (TCon (Id "->") (KArrow Star (KArrow Star Star)))

newtype Level = Level Int
    deriving (Eq, Ord, Show)

levelInc :: Level -> Level
levelInc (Level i) = Level $ i + 1
levelDec :: Level -> Level
levelDec (Level i) = Level $ i - 1 -- TODO assert > 0

data TVarLink t
    = Unbound Int Level
    | Link t
    deriving (Eq, Ord, Show)

data TypeVar v t
    = TypeVar { tyVarCell :: v (TVarLink t), tyVarKind :: Kind }

instance Show (STRef s t) where
    show v = "<stref>"

instance HasKind (TypeVar v t) where
    kind (TypeVar c k) = k

-- deriving instance Eq t => Eq (TypeVar Identity t)
-- deriving instance Show t => Show (TypeVar Identity t)
deriving instance Eq t => Eq (TypeVar (STRef s) t)
deriving instance Show t => Show (TypeVar (STRef s) t)

data TypeABT v t
    = TyVar (TypeVar v t)
    | TyAST (TypeAST t)
    -- deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriving instance Eq t => Eq (TypeABT (STRef s) t)
deriving instance Show t => Show (TypeABT (STRef s) t)

instance (HasKind t) => HasKind (TypeABT v t) where
    kind (TyVar tv) = kind tv
    kind (TyAST ast) = kind ast

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)

data SType s = SType (TypeABT (STRef s) (SType s))

deriving instance Show (TypeABT (STRef s) (SType s)) => Show (SType s)

instance HasKind (SType s) where
    kind (SType t) = kind t

data Class = Class Id Kind
    deriving (Eq, Ord, Show)

data Pred t = PredIs Class t
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

readVar :: TypeVar (STRef s) t -> Infer s (TVarLink t)
readVar (TypeVar ref k) = lift . lift $ readSTRef ref

writeVar :: TypeVar (STRef s) t -> TVarLink t -> Infer s ()
writeVar (TypeVar ref k) link = lift . lift $ writeSTRef ref link

resolve :: SType s -> Infer s (Maybe Type)
resolve (SType (TyVar tvar)) = do
    link <- readVar tvar
    case link of
        Unbound _name level -> throwError
            $ EscapedSkolemError $ "resolve" ++ show tvar ++ ", level: " ++ show level -- TODO perhaps generalize?
        Link t' -> resolve t'
resolve (SType (TyAST t)) = do
    mt <- traverse resolve t
    return . fmap Fix $ sequenceA mt

unresolve :: Type -> SType s
unresolve (Fix t) = SType . TyAST $ fmap unresolve t

varBind :: TypeVar (STRef s) (SType s) -> SType s -> Infer s ()
varBind tvar t = do
    when (kind tvar /= kind t) $ throwError $ KindMismatchError (kind tvar) (kind t)
    vt <- readVar tvar
    case vt of
        Unbound _name level -> writeVar tvar (Link t)
        Link t' -> unify t' t

unchain :: SType s -> Infer s (SType s)
unchain t@(SType (TyVar tvar)) = do
    vt <- readVar tvar
    case vt of
        Unbound{} -> return t
        Link t' -> unchain t'
unchain t = return t

unify :: SType s -> SType s -> Infer s ()
unify t1 t2 = do
    t1' <- unchain t1
    t2' <- unchain t2
    when (kind t1 /= kind t2) $ throwError $ KindMismatchError (kind t1) (kind t2)
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
unifyAST (TyGen vs1 t1) (TyGen vs2 t2) | vs1 == vs2 = unify t1 t2
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

generalizeVars :: SType s -> Infer s ([GenVar], SType s)
generalizeVars t@(SType (TyVar tvar)) = do
    link <- readVar tvar
    case link of
        Link t' -> generalizeVars t'
        Unbound name level -> do
            curLevel <- getCurrentLevel
            if curLevel < level
            then do
                let gv = GenVar name Star
                    tgenvar = SType (TyAST $ TyGenVar gv)
                writeVar tvar $ Link tgenvar
                return ([gv], tgenvar)
            else return ([], t)
generalizeVars t@(SType (TyAST (TyGenVar genVar))) =
    return ([genVar], t)
generalizeVars (SType (TyAST (TyGen genvars t))) = do
    -- erase rank > 1
    -- TODO consider failing if rank > 1 found
    (rank2Vars, t') <- generalizeVars t
    -- TODO check for collisions in genvar ids
    return (genvars `listUnion` rank2Vars, t')
generalizeVars (SType (TyAST (TyAp t1 t2))) = do
    (vs1, t1') <- generalizeVars t1
    (vs2, t2') <- generalizeVars t2
    return (vs1 `listUnion` vs2
           , SType (TyAST (TyAp t1' t2')))
generalizeVars (SType (TyAST TyCon{..})) =
    return ([], SType (TyAST TyCon{..}))
generalizeVars (SType (TyAST (TyComp c))) = do
    let FlatComposite labels mEnd = flattenComposite c
    fc' <- mapM (\(name, t) -> (name,) <$> generalizeVars t) $ Map.toList labels
    (endGVs, mEnd') <- case mEnd of
        Nothing -> return ([], Nothing)
        Just end -> do
            (endGV, endT) <- generalizeVars end
            return (endGV, Just endT)
    let genRes = map snd fc'
        c' = unflattenComposite
            $ FlatComposite ( Map.fromList
                              $ map (\(name, (_, tField)) -> (name, tField))
                              $ fc' )
            $ mEnd'
    return ( foldr listUnion [] (endGVs : map fst genRes)
           , SType (TyAST $ TyComp c'))

generalize :: SType s -> Infer s (SType s)
generalize t = do
    (genvars, t') <- generalizeVars t
    return $ case genvars of
        [] -> t'
        vs -> SType (TyAST (TyGen vs t'))

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
    (edef', edefT) <- inLevel $ do
        tvar <- freshTVar
        is <- get
        (edef', QualType edefP edefT) <- subInfer (is { isContext = Map.insert var tvar (isContext is) }) (infer edef)
        let varT = SType $ TyVar tvar
        unify varT edefT
        return (edef', edefT)

    genVarT <- generalize edefT
    tvarGen <- freshTVar
    varBind tvarGen genVarT
    is' <- get
    (expr', exprT) <- subInfer (is' { isContext = Map.insert var tvarGen (isContext is') }) (infer expr)
    return (ELet (a, exprT) var edef' expr', exprT)

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
    when (kind t /= Star) $ throwError $ KindMismatchError (kind t) Star
    traverse (qresolve . snd) expr'

