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
module Fresh.Types where

import           Fresh.Kind (Kind(..))
import qualified Fresh.Kind as Kind
import qualified Fresh.OrderedSet as OrderedSet
import           Fresh.OrderedSet (OrderedSet)
import Data.STRef
import Control.Monad (join)
import Control.Monad.ST (ST)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Functor.Identity (runIdentity)


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
    = FlatComposite { fcLabels :: Map CompositeLabelName t
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
    -- TODO: Should return ordered set so that foralls will have the
    -- genvars in deterministic order for easier alpha-equivalence
    freeGenVars :: t -> m (OrderedSet (GenVar g))

instance (Ord g, HasGen m t g) => HasGen m [t] g where
    freeGenVars ft = OrderedSet.concatUnions <$> mapM freeGenVars ft

----------------------------------------------------------------------
data ClassId = ClassId String
    deriving (Generic, Eq, Ord, Show)

data Pred t = PredIs ClassId t | PredNoLabel CompositeLabelName t
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
        return $ OrderedSet.concatUnions (gvst:gvsp)


emptyQual :: t -> QualType t
emptyQual = QualType []

----------------------------------------------------------------------

data TypeAST g t
    = TyAp { _tyApFun :: t, _tyApArg :: t }
    | TyCon { _tyCon :: TCon }
    | TyGenVar { _tyGenVar :: GenVar g }
    | TyGen { _tyGenVars :: [GenVar g], _tyGenScheme :: QualType t }
    | TyComp { _tyComp :: Composite t }
    deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Ord g, HasGen m t g) => HasGen m (TypeAST g t) g where
    freeGenVars (TyGenVar g) = pure $ OrderedSet.singleton g
    freeGenVars (TyGen gvs t) = OrderedSet.difference <$> freeGenVars t <*> pure (OrderedSet.fromList gvs)
    freeGenVars t = foldr OrderedSet.concatUnion OrderedSet.empty <$> traverse freeGenVars t

class Monad m => HasVars m t where
    freeVars :: t -> m (OrderedSet UnboundVarName)

instance HasVars m t => HasVars m (TypeAST g t) where
    freeVars t = foldr OrderedSet.concatUnion OrderedSet.empty <$> traverse freeVars t

bimapTypeAST :: (g -> g') -> (t -> t') -> TypeAST g t -> TypeAST g' t'
bimapTypeAST fg _  (TyGenVar g)  = TyGenVar (fmap fg g)
bimapTypeAST fg ft (TyGen gvs t) = TyGen (map (fmap fg) gvs) (fmap ft t)
bimapTypeAST _  ft (TyAp t1 t2)  = TyAp (ft t1) (ft t2)
bimapTypeAST _  _  TyCon{..}     = TyCon{..}
bimapTypeAST _  ft (TyComp c)    = TyComp $ fmap ft c

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
    level = genVarAnnot
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
    freeGenVars Unbound{} = pure OrderedSet.empty

instance HasGen (ST s) t g => HasGen (ST s) (TypeVar (STRef s) t) g where
    freeGenVars (TypeVar cell _) =
        readSTRef cell >>= freeGenVars

instance HasKind (TypeVar v t) where
    kind (TypeVar _ k) = Just k

instance HasVars m t => HasVars m (TVarLink t) where
    freeVars (Link t)      = freeVars t
    freeVars (Unbound n _) = pure (OrderedSet.singleton n)

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

deriving instance (Show t) => Show (TypeVar PCell t)
deriving instance (Show g, Show t) => Show (TypeABT g PCell t)

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
    | (f == Fix tyFunc) && OrderedSet.null (runIdentity (freeGenVars arg) `OrderedSet.intersection` OrderedSet.fromList gvs)
    = normalize $ Fix $ TyGen gvs (fmap (Fix . TyAp t1) q)
normalize (Fix (TyGen gvs1 (QualType ps1 (Fix (TyGen gvs2 (QualType ps2 t)))))) = normalize $ Fix (TyGen (gvs1++gvs2) $ QualType (ps1++ps2) t)
normalize t = t

normalizeQual :: QualType Type -> QualType Type
normalizeQual = fmap normalize
deriving instance Generic (f (Fix f)) => Generic (Fix f)

instance Eq g => Eq (Fix (TypeAST g)) where
    (Fix x) == (Fix y) = x == y

instance Ord g => Ord (Fix (TypeAST g)) where
    (Fix x) `compare` (Fix y) = x `compare` y


unresolveGV :: GenVar () -> GenVar Level
unresolveGV = fmap (const LevelAny)

unresolve :: Type -> SType s
unresolve (Fix t) = SType . TyAST $ bimapTypeAST (const LevelAny) unresolve t

unresolvePred :: Pred Type -> Pred (SType s)
unresolvePred = fmap unresolve

unresolveQual :: QualType Type -> QType s
unresolveQual (QualType ps t) = QualType (map unresolvePred ps) (unresolve t)

----------------------------------------------------------------------

data FlatTy t
    = FlatTyAp (FlatTy t) (FlatTy t)
    | FlatTyLeaf t

flattenTyAp :: (f -> Maybe (TypeAST a f)) -> f -> FlatTy f
flattenTyAp uncon t = case uncon t of
    Just (TyAp ap res) -> FlatTyAp (flattenTyAp uncon ap) (flattenTyAp uncon res)
    _ -> FlatTyLeaf t

flattenSTyAp :: SType s -> FlatTy (SType s)
flattenSTyAp = flattenTyAp unSType
    where
        unSType (SType (TyAST t)) = Just t
        unSType _                 = Nothing

unFlattenTy :: (TypeAST g t -> t) -> FlatTy t -> t
unFlattenTy con (FlatTyAp f1 f2) = con (TyAp (unFlattenTy con f1) (unFlattenTy con f2))
unFlattenTy _   (FlatTyLeaf t) = t

unFlattenSTy :: FlatTy (SType s) -> SType s
unFlattenSTy = unFlattenTy (SType . TyAST)


----------------------------------------------------------------------

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
    | AssertionError String
    | MultipleErrors [TypeError]
    | InstanceMethodMissing String
    | InstanceMemberWrongType String
    | CallFrame String
    deriving (Generic, Eq, Show)

concatErrors :: TypeError -> TypeError -> TypeError
concatErrors (MultipleErrors e1s) (MultipleErrors e2s) = MultipleErrors (e1s ++ e2s)
concatErrors (MultipleErrors e1s) e                    = MultipleErrors (e1s ++ [e])
concatErrors e                    (MultipleErrors e2s) = MultipleErrors (e:e2s)
concatErrors e1                   e2                   = MultipleErrors [e1,e2]


----------------------------------------------------------------------

newtype MemberName = MemberName String
    deriving (Generic, Eq, Ord, Show)

data Instance t expr = Instance { instCls :: ClassId, instType :: QualType t, instMembers :: Map MemberName expr }
    deriving (Generic, Eq, Ord, Show)

data Class t expr = Class { clsId :: ClassId
                          , clsSupers :: [ClassId]
                          , clsParam :: GenVar () -- is used in member types
                          , clsMembers :: Map MemberName (QualType t)
                          , clsInstances :: [Instance t expr]
                          }
    deriving (Generic, Eq, Ord, Show)

data ClassEnv t expr = ClassEnv { cenvClasses :: Map ClassId (Class t expr) }
    deriving (Generic, Eq, Ord, Show)

getMemberType :: Class t expr -> MemberName -> Maybe (TypeAST () t)
getMemberType cls name = TyGen [clsParam cls] <$> Map.lookup name (clsMembers cls)

--------------------------------------------------------------------------------
