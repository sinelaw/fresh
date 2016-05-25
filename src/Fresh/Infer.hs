{-# LANGUAGE RankNTypes #-}
-- |

module Fresh.Infer where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.ST (runST, ST)
import Data.STRef

import Fresh.Pretty (Pretty(..))
import Fresh.Kind (Kind(..))
import Fresh.Type
import Fresh.Unify (unify, varBind)

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


tcon :: String -> Kind -> SType s
tcon name k = SType (TyAST (TyCon (TCon (Id name) k)))

inferLit :: Lit -> SType s
inferLit LitNum{} = tcon "Number" Star
inferLit LitString{} = tcon "String" Star
inferLit LitBool{} = tcon "Bool" Star

runInferError :: InferState s -> Infer s a -> Infer s (Either TypeError (a, InferState s))
runInferError s act = lift . lift $ runEitherT $ runStateT act s

subInfer :: InferState s -> Infer s a -> Infer s a
subInfer state' act = do
    res <- runInferError state' act
    case res of
        Left err -> throwError err
        Right (x, is') -> do
            modify $ \is -> is { isGenFresh = isGenFresh is' }
            return x

withVar :: EVarName -> TypeVar (STRef s) (SType s) -> Infer s a -> Infer s a
withVar v t act = do
    is <- get
    subInfer (is { isContext = Map.insert v t (isContext is) }) act

type InferResult s a = (Expr (a, QType s), QType s)

infer :: Show a => (Expr a -> Infer s (InferResult s a)) -> Expr a -> Infer s (InferResult s a)

infer r (ELit a lit) = return (ELit (a, t) lit, t)
    where t = emptyQual $ inferLit lit

infer r (EVar a var) = do
    is <- get
    case Map.lookup var (isContext is) of
        Nothing -> throwError $ InvalidVarError (show var)
        Just ref -> return (EVar (a, t) var, t)
            where t = emptyQual $ SType $ TyVar ref

infer r (ELam a var expr) = do
    (tvar, ps, expr', exprT') <- inLevel $ do
        tvar <- freshTVar
        (expr', QualType ps exprT) <- withVar var tvar $ r expr
        exprT' <- instantiate exprT
        return (tvar, ps, expr', exprT')
    genT <- generalize $ funT (SType $ TyVar tvar) exprT'
    -- TODO check that tvar is not polymorphic (forall'd)
    let resT = QualType ps genT
    return (ELam (a, resT) var expr', resT)

infer r (EALam a var varQ expr) = do
    QualType varPs varAT <- instantiateAnnot varQ
    (ps, varAT', expr', exprT) <- inLevel $ do
        --varAT' <- instantiate varAT
        let varAT' = varAT -- TODO instantiate 'some' quantifiers (when we have them)
        tvar <- freshTVar
        varBind tvar varAT'
        (expr', QualType ps exprT) <- withVar var tvar $ r expr
        return (ps, varAT', expr', exprT)
    exprT' <- instantiate exprT
    genT <- generalize $ funT varAT' exprT'
    let resT = QualType (varPs ++ ps) genT
    return (ELam (a, resT) var expr', resT)

infer r (ELet a var edef expr) = do
    tvar <- freshTVar
    (edef', QualType edefP edefT) <- withVar var tvar $ r edef
    unify (SType $ TyVar tvar) edefT
    (expr', QualType exprP exprT) <- withVar var tvar $ r expr
    let resT = QualType (exprP ++ edefP) exprT
    return (ELet (a, resT) var edef' expr', resT)

infer r (EApp a efun earg) = do
    (efun', QualType efunP efunT) <- r efun
    (efunArg, efunRes) <- matchFun efunT
    (earg', QualType eargP eargT) <- r earg
    subsume efunArg eargT
    resT <- generalize efunRes
    let resQ = QualType (efunP ++ eargP) resT
    return (EApp (a, resQ) efun' earg', resQ)

-- Propagate annotations into Let 'manually':
infer r (EAsc a asc (ELet a' var edef expr)) = do
    -- TODO handle 'some' quanitifier in annotation
    (expr', QualType exprP exprT) <- r (ELet a' var edef (EAsc a asc expr))
    ascQ@(QualType ascP ascT) <- instantiateAnnot asc
        -- resQ = QualType (ascP ++ exprP)
    subsume ascT exprT
    return (EAsc (a, ascQ) asc expr', ascQ)

-- TODO: Propagate into EApp
infer r (EAsc a asc expr) = do
    ascQ <- instantiateAnnot asc
    (expr', exprQ) <- r (EApp a (EALam a dummy asc (EVar a dummy)) expr)
    return (EAsc (a, ascQ) asc expr', ascQ)
    where
        dummy = EVarName "x"

infer r (EGetField a expr name) = do
    (expr', QualType exprP exprT) <- r expr
    tvar <- SType . TyVar <$> freshTVar
    rvar <- SType . TyVar <$> freshRVar
    unify exprT (recT [(name, tvar)] $ Just rvar)
    let resT = QualType exprP tvar
    return (EGetField (a, resT) expr' name, resT)

instantiateAnnot :: ETypeAsc -> Infer s (QualType (SType s))
instantiateAnnot (ETypeAsc (QualType ps t)) =
    QualType (map unresolvePred ps) <$> instantiateAnnot' t

instantiateAnnot' :: Type -> Infer s (SType s)
instantiateAnnot' (Fix ascType) = do
    case ascType of
        TyGen gvs tscheme -> do
            let mkGen k = GenVar <$> freshName <*> pure k <*> pure LevelAny
                gvs' = map (fmap $ const LevelAny) gvs
            freshGVs <- mapM (mkGen . genVarKind) gvs
            tscheme' <- instantiateAnnot' tscheme
            tscheme'' <- substGens gvs' (map (SType . TyAST . TyGenVar) freshGVs) tscheme'
            return . SType . TyAST $ TyGen freshGVs tscheme''
        _ -> fmap (SType . TyAST) . sequenceA . bimapTypeAST (const LevelAny) id $ fmap instantiateAnnot' ascType

data FlatTy t
    = FlatTyAp (FlatTy t) (FlatTy t)
    | FlatTyLeaf t

flattenTyAp :: SType s -> FlatTy (SType s)
flattenTyAp (SType (TyAST (TyAp ap res))) = FlatTyAp (flattenTyAp ap) (flattenTyAp res)
flattenTyAp t = FlatTyLeaf t

unFlattenTy :: FlatTy (SType s) -> SType s
unFlattenTy (FlatTyLeaf t) = t
unFlattenTy (FlatTyAp f1 f2) = SType (TyAST (TyAp (unFlattenTy f1) (unFlattenTy f2)))

matchFun :: SType s -> Infer s (SType s, SType s)
matchFun t = instantiate t >>= matchFun'

matchFun' :: SType s -> Infer s (SType s, SType s)
matchFun' t@(SType (TyAST{}))
    | (FlatTyAp (FlatTyAp cf fArg) fRes) <- flattenTyAp t
    , SType (TyAST (TyCon c)) <- unFlattenTy cf
    , c == conFunc
    = return (unFlattenTy fArg, unFlattenTy fRes)
    | otherwise = do
      pt <- purify t
      throwError $ ExpectedFunction (show $ pretty pt)

matchFun' (SType (TyVar tvar@(TypeVar _ k))) = do
    t <- readVar tvar
    case t of
        Link t' -> matchFun t'
        Unbound _n l -> do
            arg <- SType . TyVar <$> freshTVarK k
            res <- SType . TyVar <$> freshTVarK k
            writeVar tvar (Link $ funT arg res)
            return (arg, res)

skolemize :: SType s -> Infer s ([GenVar Level], SType s)
skolemize (SType (TyAST (TyGen vs t))) = do
    ks <- mapM getKind vs
    curLevel <- getCurrentLevel
    skolems <- mapM (\k -> GenVar <$> freshName <*> pure k <*> pure curLevel) ks
    let skolemTs = map (SType . TyAST . TyGenVar) skolems
    t' <- substGens vs skolemTs t
    return (skolems, t')
skolemize t = return ([], t)

subsume :: SType s -> SType s -> Infer s ()
subsume t1 t2 = withWrap $ do
    (sks, t1') <- skolemize t1
    t2' <- instantiate t2
    unify t1' t2'
    gvs1 <- liftST $ freeGenVars t1
    gvs2 <- liftST $ freeGenVars t2
    when (not . Set.null $ (Set.fromList sks) `Set.intersection` (gvs1 `Set.union` gvs2))
        $ throwError
        $ EscapedSkolemError
        $ concat -- TODO pretty
        [ "Type not polymorphic enough to unify"
        , "\n\t", "Type 1: ", show $ pretty t1
        , "\n\t", "Type 2: ", show $ pretty t2
        ]
    where
        withWrap act = act `catchError` wrapError
        wrapError = \e -> throwError $
                          WrappedError (SubsumeError (show $ pretty t1)(show $ pretty t2)) e

runInfer :: (forall s. Infer s a) -> Either TypeError a
runInfer act =
    runST $ runEitherT $ evalStateT act InferState { isContext = Map.empty
                                                    , isGenFresh = 0
                                                    , isLevel = Level 0 }

qresolve :: QType s -> Infer s (QualType Type)
qresolve (QualType ps ti) = do
    t <- generalize ti
    mt' <- resolve t `catchError` (\e -> throwError $ WrappedError (ResolveError . show $ pretty t) e)
    pms' <- traverse (traverse resolve) ps
    let mps' = sequenceA $ map sequenceA pms'
    case (mps', mt') of
        (Just ps', Just t') -> return $ QualType ps' t'
        _ -> throwError $ EscapedSkolemError $ "qresolve:" ++ show mps' ++ " - " ++ show mt'

wrapInfer :: Show a => Expr a -> Infer s (InferResult s a)
wrapInfer expr = do
    let wrapError :: TypeError -> Infer s (InferResult s a)
        wrapError e = throwError $ WrappedError (InferenceError (show $ pretty expr)) e
    infer wrapInfer expr `catchError` wrapError

inferExpr :: Show a => Expr a -> Either TypeError (Expr (QualType Type))
inferExpr expr = runInfer $ do
    (expr', QualType p t) <- inLevel $ wrapInfer expr
    k <- getKind t
    when (k /= Star) $ throwError $ KindMismatchError k Star
    -- TODO should we really generalize? Is this func only for top-level exprs?
    t' <- generalize t
    let exprG = fmap (\(a, _) -> (a, QualType p t')) expr'
        wrapError = \e -> do
            pt <- purify t'
            throwError $ WrappedError (ResolveError (show $ pretty $ pt)) e
    traverse ((fmap $ fmap normalize) . qresolve . snd) exprG `catchError` wrapError

isRight :: Either a b -> Bool
isRight Right{} = True
isRight Left{}  = False

equivalent :: Type -> Type -> Bool
equivalent t1 t2 = isRight $ runInfer $ do
    let t1' = unresolve t1
        t2' = unresolve t2
    subsume t1' t2'
    subsume t2' t1'

equivalentPred :: Pred Type -> Pred Type -> Bool
equivalentPred p1 p2 = (fromPred p1) `equivalent` (fromPred p2)

equivalentQual :: QualType Type -> QualType Type -> Bool
equivalentQual (QualType p1 t1) (QualType p2 t2)
    | length p1 /= length p2                    = False
    | all (uncurry equivalentPred) (zip p1 p2)  = equivalent t1 t2
    | otherwise                                 = False
