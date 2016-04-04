{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Type where

import           Fresh.Kind (Kind(..))
import qualified Fresh.Kind as Kind
import Data.STRef
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Foldable

-- import Debug.Trace (traceM)

data Id = Id String
    deriving (Eq, Ord, Show)

data TCon = TCon { tcId ::  Id, tcKind :: Kind }
    deriving (Eq, Ord, Show)

data GenVar = GenVar { genVarId :: Int, genVarKind :: Kind }
    deriving (Eq, Ord, Show)

data TypeAST t
    = TyAp { _tyApFun :: t, _tyApArg :: t }
    | TyCon { _tyCon :: TCon }
    | TyGenVar { _tyGenVar :: GenVar }
    | TyGen { _tyGenVars :: [GenVar], _tyGenScheme :: t }
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
unifyAST t1 t2 = throwError $ UnificationError (show t1) (show t2)

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
    = UnificationError String String
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

[] `listUnion` y = y
x `listUnion` [] = x
x `listUnion` y = Set.toList $ (Set.fromList x) `Set.union` (Set.fromList y)

freshName :: Infer s Int
freshName = do
    is <- get
    let genId = isGenFresh is
    put $ is { isGenFresh = genId + 1 }
    return genId

generalizeVars :: SType s -> Infer s ([GenVar], (SType s))
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
generalizeVars (SType (TyAST (TyCon{..}))) =
    return ([], SType (TyAST (TyCon{..})))

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

freshTVar :: Infer s (TypeVar (STRef s) a)
freshTVar = do
    ref <- fresh
    return $ TypeVar ref Star

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

infer e@(ELam a var expr) = do
    tvar <- freshTVar
    is <- get
    let varT = SType $ TyVar tvar
        newContext = Map.insert var tvar (isContext is)
    (expr', QualType ps exprT) <- subInfer (is { isContext = newContext }) $ infer expr
    let t = QualType ps $ funT varT exprT
    return $ (ELam (a, t) var expr', t)

infer e@(EVar a var) = do
    is <- get
    case Map.lookup var (isContext is) of
        Nothing -> throwError $ InvalidVarError (show var)
        Just ref -> return (EVar (a, t) var, t)
            where t = emptyQual $ SType $ TyVar ref

infer e@(EApp a efun earg) = do
    (efun', QualType efunP efunT) <- infer efun
    (earg', QualType eargP eargT) <- infer earg
    tvar <- freshTVar
    let resT = SType $ TyVar tvar
    unify efunT (funT eargT resT)
    let resQ = QualType (efunP ++ eargP) $ resT
    return (EApp (a, resQ) efun' earg', resQ)

infer e@(ELet a var edef expr) = do
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

infer e@(EAsc a asc@(QualType ps t) expr) = do
    let st = unresolve t
        sps = map (fmap unresolve) ps
    (expr', QualType exprP exprT) <- infer expr
    unify exprT st
    let resT = QualType (sps ++ exprP) exprT
    return (EAsc (a, resT) asc expr', resT)

runInfer :: (forall s. Infer s a) -> Either TypeError a
runInfer act =
    runST $ runEitherT $ evalStateT act (InferState { isContext = Map.empty
                                                    , isGenFresh = 0
                                                    , isLevel = Level 0 })

qresolve :: QType s -> Infer s (QualType Type)
qresolve (QualType ps t) = do
    mt' <- resolve t
    pms' <- traverse (traverse resolve) ps
    let mps' = sequenceA $ map sequenceA $ pms'
    case (mps', mt') of
        (Just ps', Just t') -> return $ QualType ps' t'
        _ -> throwError $ EscapedSkolemError $ "qresolve:" ++ show mps' ++ " - " ++ show mt'

inferExpr :: Show a => Expr a -> Either TypeError (Expr (QualType Type))
inferExpr expr = runInfer $ do
    (expr', _t) <- infer expr
    traverse (qresolve . snd) expr'

