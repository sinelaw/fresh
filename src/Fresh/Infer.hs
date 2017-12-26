{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Fresh.Infer where

import           Data.Functor.Identity
import           Fresh.OrderedSet (OrderedSet)
import qualified Fresh.OrderedSet as OrderedSet

import           Control.Monad (when, forM_, unless)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
import           Control.Monad.State.Class (MonadState(..), modify)
import           Control.Monad.Trans.Either (EitherT(..), runEitherT)
import           Control.Monad.Error.Class (MonadError(..))

import qualified Data.Map as Map
import           Control.Monad.ST (runST)
import           Data.STRef

import           Fresh.Pretty (Pretty(..))
import           Fresh.Kind (Kind(..))
import           Fresh.Types
import           Fresh.Expr
import           Fresh.InferMonad
import           Fresh.Unify (unify, varBind)

funT :: SType s -> SType s -> SType s
funT targ tres =
    SType . TyAST
    $ TyAp (SType . TyAST . TyAp (SType $ TyAST tyFunc) $ targ) tres

recT :: [(CompositeLabelName, SType s)] -> Maybe (SType s) -> SType s
recT fs rest =
    SType . TyAST
    $ TyAp (SType $ TyAST tyRec) (SType . TyAST $ TyComp tcomp)
    where
        tcomp = unflattenComposite (FlatComposite (Map.fromList fs) rest)


tcon :: String -> Kind -> SType s
tcon name k = SType (TyAST (TyCon (TCon (Id name) k)))

inferLit :: (Expr a -> Infer s (InferResult s a)) -> a -> Lit a -> Infer s (InferResult s a)
inferLit r a LitNum{} = return (ELit (a, t) LitNum{}, t)
    where t = emptyQual $ tcon "Number" Star
inferLit r a LitString{} = return (ELit (a, t) LitString{}, t)
    where t = emptyQual $ tcon "String" Star
inferLit r a LitBool{} = return (ELit (a, t) LitBool{}, t)
    where t = emptyQual $ tcon "Bool" Star
inferLit r a (LitStruct []) = return (ELit (a, t) (LitStruct []), t)
    where t = emptyQual $ recT [] Nothing
inferLit r a (LitStruct rs) = do
    ts <- go rs
    let rs' = zip (map fst rs) (map fst ts)
        preds = concat (map (qualPred . snd) ts)
        ts' = zip (map fst rs) (map (qualType . snd) ts)
        t = QualType preds $ (recT ts' Nothing)
    return (ELit (a, t) (LitStruct rs'), t)
    where
        go [] = return []
        go ((fname, fexpr):rs) = do
            texpr <- r fexpr
            trs <- go rs
            return $ texpr:trs


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

infer r (ELit a lit) = inferLit r a lit

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
        QualType instPs exprT' <- instantiate exprT
        return (tvar, ps ++ instPs, expr', exprT')
    resT <- generalize ps (funT (SType $ TyVar tvar) exprT')
    -- TODO check that tvar is not polymorphic (forall'd)
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
    QualType instPs exprT' <- instantiate exprT
    resT <- generalize (varPs++instPs) $ funT varAT' exprT'
    return (EALam (a, resT) var varQ expr', resT)

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
    resQ <- generalize  (efunP ++ eargP) efunRes
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
        dummy = EVarName "_dummy_x_"

infer r (EGetField a expr name) = do
    (expr', QualType exprP exprT) <- r expr
    tvar <- SType . TyVar <$> freshTVar
    rvar <- SType . TyVar <$> freshRVar
    unify exprT (recT [(name, tvar)] $ Just rvar)
    let resT = QualType exprP tvar
    return (EGetField (a, resT) expr' name, resT)

infer _r (EBuiltIn a s asc) = do
    ascQ <- instantiateAnnot asc
    return (EBuiltIn (a, ascQ) s asc, ascQ)

instantiateAnnot :: ETypeAsc -> Infer s (QualType (SType s))
instantiateAnnot (ETypeAsc q) = callFrame "instantiateAnnot" $ do
    -- TODO: Check the predicates ps to see if they contain escaped genvars from t
    gvs <- freeGenVars q :: Infer s (OrderedSet (GenVar ()))
    let gvs' = map (fmap $ const LevelAny) $ OrderedSet.toList gvs
        QualType ps' t' = unresolveQual q
    res <- mkGenQ gvs' ps' t'
    resFreeGVs :: (OrderedSet (GenVar Level)) <- liftST $ freeGenVars res
    unless (OrderedSet.null resFreeGVs)
        $ throwError $ AssertionError ("Genvars escaped from forall'ed annotated type?! " ++ show res)
    return res

-- instantiateAnnot' :: Type -> Infer s (SType s)
-- instantiateAnnot' (Fix ascType) = do
--     case ascType of
--         TyGen gvs tscheme -> do
--             tscheme' <- instantiateAnnot' tscheme
--             tscheme'' <- substGens gvs' (map (SType . TyAST . TyGenVar) freshGVs) tscheme'
--             return . SType . TyAST $ TyGen freshGVs tscheme''
--         _ -> fmap (SType . TyAST) . sequenceA . bimapTypeAST (const LevelAny) id $ fmap instantiateAnnot' ascType

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
matchFun t = do
    QualType ps t' <- instantiate t
    -- TODO unused ps
    matchFun' t'

matchFun' :: SType s -> Infer s (SType s, SType s)
matchFun' t@(SType TyAST{})
    | (FlatTyAp (FlatTyAp cf fArg) fRes) <- flattenTyAp t
    , SType (TyAST (TyCon c)) <- unFlattenTy cf
    , c == conFunc
    = return (unFlattenTy fArg, unFlattenTy fRes)
    | otherwise = do
      pt <- purify t
      throwError $ ExpectedFunction (show $ pretty pt)

matchFun' (SType (TyVar tvar@(TypeVar _ k))) = callFrame "matchFun' (TypVar)" $ do
    t <- readVar tvar
    case t of
        Link t' -> matchFun t'
        Unbound _n l -> do
            arg <- SType . TyVar <$> freshTVarK k
            res <- SType . TyVar <$> freshTVarK k
            writeVar tvar (Link $ funT arg res)
            return (arg, res)

skolemize :: SType s -> Infer s ([GenVar Level], QualType (SType s))
skolemize (SType (TyAST (TyGen vs (QualType ps t)))) = do
    ks <- mapM getKind vs
    curLevel <- getCurrentLevel
    skolems <- mapM (\k -> GenVar <$> freshName <*> pure k <*> pure curLevel) ks
    let skolemTs = map (SType . TyAST . TyGenVar) skolems
    t' <- substGens vs skolemTs t
    ps' <- mapM (traverse (substGens vs skolemTs)) ps
    return (skolems, QualType ps' t')
skolemize t = return ([], QualType [] t)

subsume :: SType s -> SType s -> Infer s ()
subsume t1 t2 = withWrap $ do
    (sks, QualType ps1' t1') <- skolemize t1
    QualType ps2' t2' <- instantiate t2
    unify t1' t2'
    gvs1 <- liftST $ freeGenVars t1
    gvs2 <- liftST $ freeGenVars t2
    let escapingSkolems = OrderedSet.fromList sks `OrderedSet.intersection` (gvs1 `OrderedSet.concatUnion` gvs2)
    unless (OrderedSet.null escapingSkolems)
        $ throwError
        $ EscapedSkolemError
        $ concat -- TODO pretty
        [ "Type not polymorphic enough to unify"
        , "\n\t", "Type 1: ", show $ pretty t1
        , "\n\t", "Type 2: ", show $ pretty t2
        , "\n", "Skolems would escape: ", show $ pretty $ OrderedSet.toList escapingSkolems
        ]
    where
        withWrap act = act `catchError` wrapError
        wrapError = throwError . WrappedError (SubsumeError (show $ pretty t1)(show $ pretty t2))

runInfer :: (forall s. Infer s a) -> Either TypeError a
runInfer act =
    runST $ runEitherT $ evalStateT act InferState { isContext = Map.empty
                                                    , isGenFresh = 0
                                                    , isLevel = Level 0 }

qresolve :: QType s -> Infer s (QualType Type)
qresolve (QualType ps ti) = callFrame "qresolve" $ do
    t <- generalize ps ti
    let wrapError e = do
            pt <- traverse purify t
            throwError $ WrappedError (ResolveError (show (pretty pt))) e
    mt' <- sequenceA <$> traverse resolve t `catchError` wrapError
    case mt' of
        (Just t') -> return t'
        _ -> throwError $ EscapedSkolemError $ "qresolve:" ++ show mt'

wrapInfer :: Show a => Expr a -> Infer s (InferResult s a)
wrapInfer expr = do
    let wrapError :: TypeError -> Infer s (InferResult s a)
        wrapError e = throwError $ WrappedError (InferenceError (show $ pretty expr)) e
    infer wrapInfer expr `catchError` wrapError

inferExprAct :: Show a => Expr a -> Infer s (Expr (a, QualType (SType s)))
inferExprAct expr = callFrame "inferExprAct" $ do
    res@(expr', (QualType p t)) <- inLevel $ wrapInfer expr
    k <- getKind t
    when (k /= Star) $ throwError $ KindMismatchError k Star
    return expr'

inferExpr :: Show a => Expr a -> Either TypeError (Expr (QualType Type))
inferExpr expr = runInfer $ callFrame "inferExpr" $ do
    exprG <- inferExprAct expr
    traverse (qresolve . snd) exprG

isRight :: Either a b -> Bool
isRight Right{} = True
isRight Left{}  = False

trySubsume :: Type -> Type -> Either TypeError ()
trySubsume t1 t2 = runInfer $ do
    let t1' = unresolve t1
        t2' = unresolve t2
    subsume t1' t2'

canSubsume :: Type -> Type -> Either TypeError ()
canSubsume = trySubsume

equivalent :: Type -> Type -> Either TypeError ()
equivalent t1 t2 = case (canSubsume t1 t2, canSubsume t2 t1) of
    (Left e1, Left e2) -> Left (concatErrors e1 e2)
    (Left e,  _      ) -> Left e
    (_     , Left e  ) -> Left e
    _                  -> Right ()

equivalentPred :: Pred Type -> Pred Type -> Either TypeError ()
equivalentPred p1 p2 = fromPred p1 `equivalent` fromPred p2

equivalentQual' :: QualType Type -> QualType Type -> Either TypeError ()
equivalentQual' (QualType p1 t1) (QualType p2 t2)
    | length p1 /= length p2                    = Left $ AssertionError "Predicates not the same length"
    | all (isRight . uncurry equivalentPred) (zip p1 p2)  = equivalent t1 t2
    | otherwise                                 = Left $ AssertionError "Not equivalent predicates"

equivalentQual :: QualType Type -> QualType Type -> Either TypeError ()
equivalentQual q1 q2 = equivalentQual' (normalizeQual q1) (normalizeQual q2)

checkClassInstance :: Show a => Class Type expr -> Instance t (Expr a) -> Either TypeError ()
checkClassInstance cls inst = runInfer $ do
    forM_ (Map.toList $ instMembers inst) $ \(name, expr) -> do
        case Map.lookup name (clsMembers cls) of
            Nothing -> throwError $ InstanceMethodMissing (show $ pretty name)
            Just q -> do
                qt <- mkGen [const LevelAny <$> clsParam cls] [] (qualType $ unresolveQual q)
                expr' <- inferExprAct expr
                subsume (qualType $ snd $ getAnnotation expr') qt

checkClass :: (Show a) => Class Type (Expr a) -> Either TypeError ()
checkClass cls =
    runIdentity
    $ runEitherT
    $ foldr (>>) (return ())
    $ map (EitherT . return . checkClassInstance cls) (clsInstances cls)
