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
import Control.Monad (join, foldM)
import Control.Monad.ST (ST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

import Data.Functor.Identity (runIdentity)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..))
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
    | TyGen { _tyGenVars :: [GenVar], _tyGenScheme :: t }
    | TyComp { _tyComp :: Composite t }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class Monad m => HasGen m t where
    freeGenVars :: t -> m (Set GenVar)

instance HasGen m t => HasGen m (TypeAST t) where
    freeGenVars (TyGenVar g) = pure $ Set.singleton g
    freeGenVars (TyGen gvs t) = Set.difference <$> freeGenVars t <*> pure (Set.fromList gvs)
    freeGenVars t = foldr Set.union Set.empty <$> traverse freeGenVars t


tyRec :: TypeAST t
tyRec = TyCon (TCon (Id "Rec") (KArrow Composite Star))

tySum :: TypeAST t
tySum = TyCon (TCon (Id "Sum") (KArrow Composite Star))

conFunc = TCon (Id "->") (KArrow Star (KArrow Star Star))

tyFunc :: TypeAST t
tyFunc = TyCon conFunc

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

instance HasGen m t => HasGen m (TypeVar PCell t) where
    freeGenVars (TypeVar (PCell t) _) = freeGenVars t

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

instance (HasGen m t, HasGen m (TypeVar v t)) => HasGen m (TypeABT v t) where
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

-- Pure cells
data PCell a = PCell a
     deriving (Show)

data PType = PType (TypeABT PCell PType)

deriving instance Show (TypeABT PCell PType) => Show PType

instance HasKind PType where
    kind (PType t) = kind t

instance (Monad m) => HasGen m PType where
    freeGenVars (PType t) = freeGenVars t

----------------------------------------------------------------------

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

normalize :: Type -> Type
normalize (Fix (TyAp t1@(Fix (TyAp f arg)) (Fix (TyGen gvs t))))
    | (f == Fix tyFunc) && (Set.null $ runIdentity (freeGenVars arg) `Set.intersection` (Set.fromList gvs))
    = Fix $ TyGen gvs (Fix $ TyAp t1 t)
normalize t = t

instance Eq (Fix TypeAST) where
    (Fix x) == (Fix y) = x == y

deriving instance Ord (Fix TypeAST)

liftST :: ST s a -> Infer s a
liftST = lift . lift

readVar :: TypeVar (STRef s) t -> Infer s (TVarLink t)
readVar (TypeVar ref k) = liftST $ readSTRef ref

writeVar :: TypeVar (STRef s) t -> TVarLink t -> Infer s ()
writeVar (TypeVar ref k) link = liftST $ writeSTRef ref link

purify :: SType s -> Infer s PType
purify (SType (TyVar tvar@(TypeVar _ k))) = do
    link <- readVar tvar
    case link of
        Unbound name l -> return . PType . TyVar $ TypeVar (PCell $ Unbound name l) k
        Link t -> purify t
purify (SType (TyAST t)) = PType . TyAST <$> traverse purify t

resolve :: SType s -> Infer s (Maybe Type)
resolve (SType (TyVar tvar)) = do
    link <- readVar tvar
    case link of
        Unbound _name l -> throwError
            $ EscapedSkolemError $ "resolve " ++ show tvar ++ ", level: " ++ show l
        Link t' -> resolve t'
resolve (SType (TyAST (TyGen gv t))) = do
    inLevel $ do
        t' <- resolve t
        return $ Fix . TyGen gv <$> t'
resolve (SType (TyAST (TyGenVar gv))) = do
    --curGenLevel <- levelDec <$> getCurrentLevel
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

unresolvePred :: Pred Type -> Pred (SType s)
unresolvePred = fmap unresolve

unresolveQual :: QualType Type -> QType s
unresolveQual (QualType ps t) = QualType (map unresolvePred ps) (unresolve t)

checkKind :: Maybe Kind -> Infer s Kind
checkKind Nothing  = throwError InvalidKind
checkKind (Just k) = return k

getKind :: HasKind t => t -> Infer s Kind
getKind = checkKind . kind


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
    | EALam a EVarName (QualType Type) (Expr a)
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

data InferState s
    = InferState
      { isContext :: Map EVarName (TypeVar (STRef s) (SType s))
      , isGenFresh :: Int
      , isLevel :: Level
      }
    deriving Show

data TypeError
    = WrappedError TypeError TypeError
    | ResolveError String
    | UnificationError String String
    | RowEndError String
    | InferenceError String
    | EscapedSkolemError String
    | InvalidKind
    | KindMismatchError Kind Kind
    | InvalidVarError String
    | ExpectedFunction String
    | SubsumeError String String
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
        Unbound _ l -> if curLevel < l
                       then pure [tv]
                       else pure []
        Link t' -> getUnbound curLevel t'
getUnbound curLevel (SType (TyAST t)) =
    foldr (++) [] <$> traverse (getUnbound curLevel) t


mkGen :: [GenVar] -> SType s -> SType s
mkGen gvs (SType (TyAST (TyGen gvs' t))) = mkGen (gvs++gvs') t
mkGen [] t = t
mkGen gvs t = SType (TyAST (TyGen gvs t))

generalize :: SType s -> Infer s (SType s)
generalize t = do
    curLevel <- getCurrentLevel
    unboundTVars <- getUnbound curLevel t
    let wrapGen tv@(TypeVar _ k) = do
            res <- readVar tv
            case res of
                Link (SType (TyAST (TyGenVar _))) -> return Nothing -- already overwritten
                Link _ -> error "Assertion failed"
                Unbound{} -> do
                    gv <- GenVar <$> freshName <*> pure k <*> pure curLevel
                    writeVar tv (Link $ SType $ TyAST $ TyGenVar gv)
                    return $ Just gv
    gvs <- catMaybes <$> mapM wrapGen unboundTVars
    return $ mkGen gvs t

instantiate :: SType s -> Infer s (SType s)
instantiate (SType (TyAST (TyGen gvs tGen))) = do
    let inst t gv@(GenVar n k l) = do
            tv <- SType . TyVar <$> freshTVarK k
            substGen gv tv t
    foldM inst tGen gvs
instantiate t@(SType (TyAST _)) = return t
instantiate t@(SType (TyVar tvar)) = do
    t' <- readVar tvar
    case t' of
        Unbound{} -> return t
        Link tLink -> instantiate tLink

substGen :: GenVar -> SType s -> SType s -> Infer s (SType s)
substGen gv tv t@(SType (TyVar tv')) = do
    t' <- readVar tv'
    case t' of
        Unbound{} -> return t
        Link tLink -> substGen gv tv tLink
substGen gv tv t@(SType (TyAST tast)) =
    case tast of
         TyGenVar g -> return $ if g == gv then tv else t
         TyAp tf tx -> SType . TyAST <$> (TyAp <$> substGen gv tv tf <*> substGen gv tv tx)
         TyCon c -> return . SType . TyAST $ TyCon c
         TyGen gvs tGen' -> SType . TyAST . TyGen gvs <$> substGen gv tv tGen'
         TyComp c -> SType . TyAST . TyComp <$> traverse (substGen gv tv) c

substGens :: [GenVar] -> [SType s] -> SType s -> Infer s (SType s)
substGens vs ts t = foldM (\t' (v,s) -> substGen v s t') t $ zip vs ts

fresh :: Infer s (STRef s (TVarLink t))
fresh = do
    curLevel <- getCurrentLevel
    name <- freshName
    lift . lift $ newSTRef $ Unbound name curLevel

freshTVarK :: Kind -> Infer s (TypeVar (STRef s) a)
freshTVarK k = do
    ref <- fresh
    return $ TypeVar ref k

freshTVar :: Infer s (TypeVar (STRef s) a)
freshTVar = freshTVarK Star

freshRVar :: Infer s (TypeVar (STRef s) a)
freshRVar = freshTVarK Composite



