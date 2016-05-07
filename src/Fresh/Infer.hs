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
import Control.Monad.ST (runST)
import Data.STRef

import Fresh.Pretty (Pretty(..))
import Fresh.Kind (Kind(..))
import Fresh.Type (TypeAST(..), TypeABT(..), TCon(..), SType(..), Infer, HasGen(..),
                   TypeError(..), inLevel, generalize, resolve, unresolveQual, Level(..), Type, getKind, liftST,
                   Id(..), freshTVar, freshTVarK, QualType(..), CompositeLabelName(..), GenVar(..), freshName, getCurrentLevel, substGens,
                   TypeVar(..), instantiate, readVar, writeVar, TVarLink(..), purify,
                   freshRVar, FlatComposite(..), unflattenComposite, EVarName(..),
                   InferState(..), Expr(..), QType, emptyQual, Lit(..))
import Fresh.Unify (unify, varBind)

tyRec :: TypeAST t
tyRec = TyCon (TCon (Id "Rec") (KArrow Composite Star))

tySum :: TypeAST t
tySum = TyCon (TCon (Id "Sum") (KArrow Composite Star))

conFunc = TCon (Id "->") (KArrow Star (KArrow Star Star))

tyFunc :: TypeAST t
tyFunc = TyCon conFunc


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

subInfer :: InferState s -> Infer s a -> Infer s a
subInfer state' act = do
    res <- lift . lift $ runEitherT $ runStateT act state'
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
    let QualType varPs varAT = unresolveQual varQ
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

--infer r (EAsc a asc expr) = do
--    infer r (EApp (EALam

infer r (EGetField a expr name) = do
    (expr', QualType exprP exprT) <- r expr
    tvar <- SType . TyVar <$> freshTVar
    rvar <- SType . TyVar <$> freshRVar
    unify exprT (recT [(name, tvar)] $ Just rvar)
    let resT = QualType exprP tvar
    return (EGetField (a, resT) expr' name, resT)


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

skolemize :: SType s -> Infer s ([GenVar], SType s)
skolemize (SType (TyAST (TyGen vs t))) = do
    ks <- mapM getKind vs
    curLevel <- getCurrentLevel
    skolems <- mapM (\k -> GenVar <$> freshName <*> pure k <*> pure curLevel) ks
    let skolemTs = map (SType . TyAST . TyGenVar) skolems
    t' <- substGens vs skolemTs t
    return (skolems, t')
skolemize t = return ([], t)

subsume :: SType s -> SType s -> Infer s ()
subsume t1 t2 = do
    (sks, t1') <- skolemize t1
    t2' <- instantiate t2
    unify t1' t2'
    gvs1 <- liftST $ freeGenVars t1'
    gvs2 <- liftST $ freeGenVars t2'
    when (not . Set.null $ (Set.fromList sks) `Set.intersection` (gvs1 `Set.union` gvs2))
        $ throwError
        $ EscapedSkolemError
        $ concat -- TODO pretty
        [ "Type not polymorphic enough to unify"
        , "\n\t", "Type 1: ", show t1
        , "\n\t", "Type 2: ", show t2
        ]

runInfer :: (forall s. Infer s a) -> Either TypeError a
runInfer act =
    runST $ runEitherT $ evalStateT act InferState { isContext = Map.empty
                                                    , isGenFresh = 0
                                                    , isLevel = Level 0 }

qresolve :: QType s -> Infer s (QualType Type)
qresolve (QualType ps ti) = do
    t <- generalize ti
    mt' <- resolve t
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
    (expr', t) <- wrapInfer expr
    k <- getKind t
    when (k /= Star) $ throwError $ KindMismatchError k Star
    traverse (qresolve . snd) expr'

