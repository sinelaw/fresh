{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Monad (join, foldM, forM)
import Control.Monad.ST (ST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Data.Functor.Identity (runIdentity)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Foldable
import qualified Data.List as List
-- import Debug.Trace (traceM)

partitionM :: Monad m => (t -> m Bool) -> [t] -> m ([t], [t])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    (y, n) <- partitionM f xs
    res <- f x
    return $ if res then (x:y, n) else (y, x:n)

data Level = Level Int | LevelAny
    deriving (Generic, Eq, Show)

instance Ord Level where
    (Level x) `compare` (Level y) = x `compare` y
    LevelAny `compare` LevelAny = EQ
    _ `compare` LevelAny = LT
    LevelAny `compare` _ = GT

mapLevel :: (Int -> Int) -> Level -> Level
mapLevel  f (Level x) = Level (f x)
mapLevel _f l         = l

levelInc :: Level -> Level
levelInc = mapLevel (+1)
levelDec :: Level -> Level
levelDec = mapLevel (\x -> x - 1) -- TODO assert > 0

data Id = Id String
    deriving (Generic, Eq, Ord, Show)

data TCon = TCon { tcId ::  Id, tcKind :: Kind }
    deriving (Generic, Eq, Ord, Show)

data GenVar a
    = GenVar
      { genVarId :: Int
      , genVarKind :: Kind
      , genVarAnnot :: a
      }
    deriving (Generic, Eq, Ord, Show, Functor)

genDropAnnot :: GenVar a -> GenVar ()
genDropAnnot gv = gv { genVarAnnot = () }

newtype CompositeLabelName = CompositeLabelName String
    deriving (Generic, Eq, Ord, Show)

data Composite t
    = CompositeLabel CompositeLabelName t (Composite t)
    | CompositeTerminal
    | CompositeRemainder t
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

data FlatComposite t
    = FlatComposite { fcLabels :: (Map CompositeLabelName t)
                    , fcRemainder :: Maybe t }
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

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

----------------------------------------------------------------------
class HasKind t where
    kind :: t -> Maybe Kind -- Should really be just Kind, but hard to generate arbitrary for TyAp

class Monad m => HasGen m t g where
    freeGenVars :: t -> m (Set (GenVar g))

----------------------------------------------------------------------

data Class = Class Id Kind
    deriving (Generic, Eq, Ord, Show)

data Pred t = PredIs Class t | PredNoLabel CompositeLabelName t
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasGen m t g => HasGen m (Pred t) g where
    freeGenVars (PredIs _ t) = freeGenVars t
    freeGenVars (PredNoLabel _ t) = freeGenVars t

fromPred :: Pred t -> t
fromPred (PredIs _ x) = x
fromPred (PredNoLabel _ x) = x

data QualType t = QualType { qualPred :: [Pred t], qualType :: t }
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasKind t => HasKind (QualType t) where
    kind (QualType _ t) = kind t

instance (Ord g, HasGen m t g) => HasGen m (QualType t) g where
    freeGenVars (QualType ps t) = do
        gvsp <- mapM freeGenVars ps
        gvst <- freeGenVars t
        return $ Set.unions (gvst:gvsp)


emptyQual :: t -> QualType t
emptyQual t = QualType [] t

----------------------------------------------------------------------

data TypeAST g t
    = TyAp { _tyApFun :: t, _tyApArg :: t }
    | TyCon { _tyCon :: TCon }
    | TyGenVar { _tyGenVar :: GenVar g }
    | TyGen { _tyGenVars :: [GenVar g], _tyGenScheme :: QualType t }
    | TyComp { _tyComp :: Composite t }
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Ord g, HasGen m t g) => HasGen m (TypeAST g t) g where
    freeGenVars (TyGenVar g) = pure $ Set.singleton g
    freeGenVars (TyGen gvs t) = Set.difference <$> freeGenVars t <*> pure (Set.fromList gvs)
    freeGenVars t = foldr Set.union Set.empty <$> traverse freeGenVars t

class Monad m => HasVars m t where
    freeVars :: t -> m (Set UnboundVarName)

instance HasVars m t => HasVars m (TypeAST g t) where
    freeVars t = foldr Set.union Set.empty <$> traverse freeVars t

bimapTypeAST :: (g -> g') -> (t -> t') -> TypeAST g t -> TypeAST g' t'
bimapTypeAST fg _  (TyGenVar g) = TyGenVar (fmap fg g)
bimapTypeAST fg ft (TyGen gvs t) = TyGen (map (fmap fg) gvs) (fmap ft t)
bimapTypeAST _ ft (TyAp t1 t2) = TyAp (ft t1) (ft t2)
bimapTypeAST _ _ (TyCon{..}) = TyCon{..}
bimapTypeAST _ ft (TyComp c) = TyComp $ fmap ft c

tyRec :: TypeAST g t
tyRec = TyCon (TCon (Id "Rec") (KArrow Composite Star))

tySum :: TypeAST g t
tySum = TyCon (TCon (Id "Sum") (KArrow Composite Star))

conFunc :: TCon
conFunc = TCon (Id "->") (KArrow Star (KArrow Star Star))

tyFunc :: TypeAST g t
tyFunc = TyCon conFunc

class HasLevel t where
    level :: t -> Level

instance HasLevel Level where
    level = id

instance HasLevel (GenVar Level) where
    level g = genVarAnnot g
instance HasLevel t => HasLevel (TypeAST g t) where
    level = foldr (min . level) LevelAny

instance HasKind TCon where
    kind = Just . tcKind
instance HasKind (GenVar a) where
    kind = Just . genVarKind
instance HasKind t => HasKind (TypeAST g t) where
    kind (TyAp f x) = join $ Kind.app <$> kind f <*> kind x
    kind (TyCon tc) = kind tc
    kind (TyGenVar gv) = kind gv
    kind (TyGen _gvs s) = kind s
    kind (TyComp _fs) = Just Composite

type UnboundVarName = Int

data TVarLink t
    = Unbound UnboundVarName Level
    | Link t
    deriving (Generic, Eq, Ord, Show, Functor)

data TypeVar v t
    = TypeVar { tyVarCell :: v (TVarLink t), tyVarKind :: Kind }
    deriving (Generic, Functor)

instance Show (STRef s t) where
    show _v = "<stref>"

instance HasGen m t g => HasGen m (TVarLink t) g where
    freeGenVars (Link t)  = freeGenVars t
    freeGenVars Unbound{} = pure Set.empty

instance HasGen (ST s) t g => HasGen (ST s) (TypeVar (STRef s) t) g where
    freeGenVars (TypeVar cell _) =
        readSTRef cell >>= freeGenVars

instance HasKind (TypeVar v t) where
    kind (TypeVar c k) = Just k

instance HasVars m t => HasVars m (TVarLink t) where
    freeVars (Link t)      = freeVars t
    freeVars (Unbound n _) = pure (Set.singleton n)

instance HasVars (ST s) t => HasVars (ST s) (TypeVar (STRef s) t) where
    freeVars (TypeVar cell _) =
        readSTRef cell >>= freeVars

-- deriving instance Eq t => Eq (TypeVar Identity t)
-- deriving instance Show t => Show (TypeVar Identity t)
deriving instance Eq t => Eq (TypeVar (STRef s) t)
deriving instance Show t => Show (TypeVar (STRef s) t)

data TypeABT g v t
    = TyVar (TypeVar v t)
    | TyAST (TypeAST g t)
    deriving (Generic, Functor)

deriving instance (Eq g, Eq t) => Eq (TypeABT g (STRef s) t)
deriving instance (Show g, Show t) => Show (TypeABT g (STRef s) t)

instance (HasKind t) => HasKind (TypeABT g v t) where
    kind (TyVar tv) = kind tv
    kind (TyAST ast) = kind ast

instance (Ord g, HasGen m t g, HasGen m (TypeVar v t) g) => HasGen m (TypeABT g v t) g where
    freeGenVars (TyVar tv)  = freeGenVars tv
    freeGenVars (TyAST ast) = freeGenVars ast

instance (Ord g, HasVars m t, HasVars m (TypeVar v t)) => HasVars m (TypeABT g v t) where
    freeVars (TyVar tv)  = freeVars tv
    freeVars (TyAST ast) = freeVars ast

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)

instance HasKind (f (Fix f)) => HasKind (Fix f) where
    kind (Fix t) = kind t

instance HasGen m (f (Fix f)) g => HasGen m (Fix f) g where
    freeGenVars (Fix t) = freeGenVars t

data SType s = SType (TypeABT Level (STRef s) (SType s))

deriving instance Show (TypeABT Level (STRef s) (SType s)) => Show (SType s)

instance HasKind (SType s) where
    kind (SType t) = kind t

instance HasGen (ST s) (SType s) Level where
    freeGenVars (SType t) = freeGenVars t

instance HasVars (ST s) (SType s) where
    freeVars (SType t) = freeVars t

-- Pure cells
data PCell a = PCell a
     deriving (Generic, Show)

instance HasGen m t g => HasGen m (TypeVar PCell t) g where
    freeGenVars (TypeVar (PCell t) _) = freeGenVars t

data PType = PType (TypeABT Level PCell PType)

deriving instance Show (TypeABT Level PCell PType) => Show PType

instance HasKind PType where
    kind (PType t) = kind t

instance (Monad m) => HasGen m PType Level where
    freeGenVars (PType t) = freeGenVars t

----------------------------------------------------------------------

type QType s = QualType (SType s)

type Type = Fix (TypeAST ())

normalize :: Type -> Type
normalize (Fix (TyAp t1@(Fix (TyAp f arg)) (Fix (TyGen gvs q))))
    | (f == Fix tyFunc) && (Set.null $ runIdentity (freeGenVars arg) `Set.intersection` (Set.fromList gvs))
    = Fix $ TyGen gvs (fmap (Fix . TyAp t1) q)
normalize (Fix (TyGen gvs1 (QualType ps1 (Fix (TyGen gvs2 (QualType ps2 t)))))) = normalize $ Fix (TyGen (gvs1++gvs2) $ QualType (ps1++ps2) t)
normalize t = t

normalizeQual :: QualType Type -> QualType Type
normalizeQual = fmap normalize
deriving instance Generic (f (Fix f)) => Generic (Fix f)

instance Eq g => Eq (Fix (TypeAST g)) where
    (Fix x) == (Fix y) = x == y

instance Ord g => Ord (Fix (TypeAST g)) where
    (Fix x) `compare` (Fix y) = x `compare` y

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
resolve (SType (TyAST t)) = do
    mt <- traverse resolve t
    return $ fmap Fix $ sequenceA $ bimapTypeAST (const ()) id mt

unresolveGV :: GenVar () -> GenVar Level
unresolveGV = fmap (const LevelAny)

unresolve' :: Type -> SType s
unresolve' (Fix t) = SType . TyAST $ bimapTypeAST (const LevelAny) unresolve' t

unresolve :: Type -> Infer s (SType s)
unresolve = return . unresolve'

unresolvePred :: Pred Type -> Infer s (Pred (SType s))
unresolvePred = return . fmap unresolve'

unresolveQual :: QualType Type -> Infer s (QType s)
unresolveQual (QualType ps t) = QualType <$> (traverse unresolvePred ps) <*> (unresolve t)

checkKind :: Maybe Kind -> Infer s Kind
checkKind Nothing  = throwError InvalidKind
checkKind (Just k) = return k

getKind :: HasKind t => t -> Infer s Kind
getKind = checkKind . kind


----------------------------------------------------------------------

newtype ETypeAsc = ETypeAsc (QualType Type)
    deriving (Generic, Eq, Ord, Show)

data EVarName = EVarName String
    deriving (Generic, Eq, Ord, Show)

data Lit
    = LitNum Double
    | LitString String
    | LitBool Bool
    deriving (Generic, Eq, Ord, Show)

data Expr a
    = ELit a Lit
    | EVar a EVarName
    | ELam a EVarName (Expr a)
    | EALam a EVarName ETypeAsc (Expr a)
    | EApp a (Expr a) (Expr a)
    | ELet a EVarName (Expr a) (Expr a)
    | EAsc a ETypeAsc (Expr a)
    | EGetField a (Expr a) CompositeLabelName
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)


getAnnotation :: Expr a -> a
getAnnotation = head . Data.Foldable.toList

-- type FExpr = Fix Expr
--     deriving (Generic, Eq, Ord, Show)

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
    | OccursError String String
    deriving (Generic, Eq, Show)

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


mkGen :: [GenVar Level] -> [Pred (SType s)] -> SType s -> SType s
mkGen gvs ps (SType (TyAST (TyGen gvs' (QualType ps2 t)))) = mkGen (gvs++gvs') (ps++ps2) t
mkGen []  [] t = t
mkGen gvs ps t = SType (TyAST (TyGen gvs (QualType ps t)))

mkGenQ :: [GenVar Level] -> [Pred (SType s)] -> SType s -> Infer s (QualType (SType s))
mkGenQ gvs ps t = do
    let gvsSet = Set.fromList gvs
    (psNotInT, psInT) <- partitionM
        (\p -> do gvsInP <- liftST (freeGenVars p)
                  return $ Set.null $ gvsInP `Set.intersection` gvsSet)
        ps
    return $ QualType psNotInT $ mkGen gvs psInT t

generalize :: [Pred (SType s)] -> SType s -> Infer s (QualType (SType s))
generalize ps t = do
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
    mkGenQ gvs ps t

instantiate :: SType s -> Infer s (QualType (SType s))
instantiate (SType (TyAST (TyGen gvs (QualType ps tGen)))) = do
    let inst t gv@(GenVar n k l) = do
            tv <- SType . TyVar <$> freshTVarK k
            substGen gv tv t
    QualType ps <$> foldM inst tGen gvs
instantiate t@(SType (TyAST _)) = return $ QualType [] t
instantiate t@(SType (TyVar tvar)) = do
    t' <- readVar tvar
    case t' of
        Unbound{} -> return $ QualType [] t -- TODO: Keep predicates on metavars?
        Link tLink -> instantiate tLink

substGen :: GenVar Level -> SType s -> SType s -> Infer s (SType s)
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
         TyGen gvs (QualType ps tGen') -> do
             let (shadowedGVs, rest) = List.partition (\sgv -> genVarId sgv == genVarId gv) gvs
             (newGVs, newTypes) <-
                 unzip <$> forM shadowedGVs (\sgv -> do
                                                    name <- freshName
                                                    let sgv' = sgv { genVarId = name }
                                                    return (sgv', SType . TyAST . TyGenVar $ sgv'))

             stGen' <- substGens shadowedGVs newTypes tGen'
             ps' <- mapM (traverse $ substGens shadowedGVs newTypes) ps
             return $ mkGen (newGVs ++ rest) ps' stGen'
         TyComp c -> SType . TyAST . TyComp <$> traverse (substGen gv tv) c

substGens :: [GenVar Level] -> [SType s] -> SType s -> Infer s (SType s)
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



