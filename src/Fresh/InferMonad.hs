{-# LANGUAGE FlexibleContexts #-}
module Fresh.InferMonad where

import Control.Monad (when, foldM, forM)

import           Data.Map (Map)
import qualified Data.Set as Set
import Data.STRef
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Error.Class (MonadError(..))

import qualified Data.List as List
import Control.Monad.ST (ST)
import Data.Maybe (catMaybes, fromMaybe)

import qualified Fresh.OrderedSet as OrderedSet

import Fresh.Types
import Fresh.Expr
import Fresh.Kind

data InferState s
    = InferState
      { isContext :: Map EVarName (TypeVar (STRef s) (SType s))
      , isGenFresh :: Int
      , isLevel :: Level
      }
    deriving Show

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
x `listUnion` y = OrderedSet.toList $ OrderedSet.fromList x `OrderedSet.concatUnion` OrderedSet.fromList y

freshName :: Infer s Int
freshName = do
    is <- get
    let genId = isGenFresh is
    put $ is { isGenFresh = genId + 1 }
    return genId

-- TODO should return a set
getUnbound :: Maybe Level -> SType s -> Infer s [TypeVar (STRef s) (SType s)]
getUnbound mCurLevel (SType (TyVar tv)) = do
    v <- readVar tv
    case v of
        Unbound _ l -> case mCurLevel of
            Just curLevel | curLevel >= l -> pure []
            _ -> pure [tv]
        Link t' -> getUnbound mCurLevel t'
getUnbound mCurLevel (SType (TyAST t)) =
    concat <$> traverse (getUnbound mCurLevel) t

mkGen :: [GenVar Level] -> [Pred (SType s)] -> SType s -> Infer s (SType s)
mkGen gvs ps (SType (TyAST (TyGen gvs' (QualType ps2 t)))) = mkGen (gvs++gvs') (ps++ps2) t
-- mkGen gvs ps touter@(SType (TyAST (TyAp t1@(SType (TyAST (TyAp (SType (TyAST (TyCon f))) arg))) (SType (TyAST (TyGen gvs' (QualType ps' t)))))))
--     = do gvsArg <- liftST $ freeGenVars arg
--          if (f == conFunc) && (OrderedSet.null $ gvsArg `OrderedSet.intersection` (OrderedSet.fromList gvs'))
--              then mkGen (gvs++gvs') (ps++ps') (SType . TyAST $ TyAp t1 t)
--              else return touter
--mkGen gvs ps (SType (TyAST (TyAp t1 (SType (TyAST (TyGen gvs' (QualType ps2 t))))))) = mkGen (gvs++gvs') (ps++ps2) (SType (TyAST (TyAp t1 t)))
mkGen []  [] t = return t
mkGen gvs ps t = do
    freeGVs <-liftST $ freeGenVars (QualType ps t)
    when (not $ Set.fromList gvs `Set.isSubsetOf` OrderedSet.toSet freeGVs) $
        throwError $ AssertionError $ "Non-existing GenVars appears in TyGen?! " ++ show gvs ++ " in type " ++ show t ++ ", freeGVs: " ++ show freeGVs
    return $ SType (TyAST (TyGen (OrderedSet.toList freeGVs) (QualType ps t)))

mkGenQ :: [GenVar Level] -> [Pred (SType s)] -> SType s -> Infer s (QualType (SType s))
mkGenQ gvs ps t = do
    let gvsSet = OrderedSet.fromList gvs
    (psNotInT, psInT) <- partitionM
        (\p -> do gvsInP <- liftST (freeGenVars p)
                  return $ OrderedSet.null $ gvsInP `OrderedSet.intersection` gvsSet)
        ps
    QualType psNotInT <$> mkGen gvs psInT t

generalizeAtLevel :: [Pred (SType s)] -> SType s -> Maybe Level -> Infer s (QualType (SType s))
generalizeAtLevel ps t mCurLevel = do
    unboundTVarsT <- getUnbound mCurLevel t
    unboundTVarsPS <- concatMap fromPred <$> mapM (traverse $ getUnbound mCurLevel) ps
    let unboundTVars = unboundTVarsT ++ unboundTVarsPS
    let wrapGen tv@(TypeVar _ k) = do
            res <- readVar tv
            case res of
                Link (SType (TyAST (TyGenVar _))) -> return Nothing -- already overwritten
                Link _ -> error "Assertion failed"
                Unbound{} -> do
                    gv <- GenVar <$> freshName <*> pure k <*> pure (fromMaybe LevelAny mCurLevel)
                    writeVar tv (Link $ SType $ TyAST $ TyGenVar gv)
                    return $ Just gv
    gvs <- catMaybes <$> mapM wrapGen unboundTVars
    mkGenQ gvs ps t

generalize :: [Pred (SType s)] -> SType s -> Infer s (QualType (SType s))
generalize ps t = callFrame "generalize" $ getCurrentLevel >>= (generalizeAtLevel ps t . Just)

instantiate :: SType s -> Infer s (QualType (SType s))
instantiate (SType (TyAST (TyGen gvs (QualType ps tGen)))) = callFrame "instantiate" $ do
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
             mkGen (newGVs ++ rest) ps' stGen'
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


liftST :: ST s a -> Infer s a
liftST = lift . lift

readVar :: TypeVar (STRef s) t -> Infer s (TVarLink t)
readVar (TypeVar ref _) = liftST $ readSTRef ref

writeVar :: TypeVar (STRef s) t -> TVarLink t -> Infer s ()
writeVar (TypeVar ref _) link = liftST $ writeSTRef ref link

purifyVar :: TypeVar (STRef s) (SType s) -> Infer s PType
purifyVar tvar@(TypeVar _ k) = do
    link <- readVar tvar
    case link of
        Unbound name l -> return . PType . TyVar $ TypeVar (PCell $ Unbound name l) k
        Link t -> purify t

purify :: SType s -> Infer s PType
purify (SType (TyVar tvar)) = purifyVar tvar
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

checkKind :: Maybe Kind -> Infer s Kind
checkKind Nothing  = throwError InvalidKind
checkKind (Just k) = return k

getKind :: HasKind t => t -> Infer s Kind
getKind = checkKind . kind

callFrame :: MonadError TypeError m => String -> m a -> m a
callFrame s act = act `catchError` (\e -> throwError $ WrappedError (CallFrame s) e)

