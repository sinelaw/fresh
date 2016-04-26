{-# LANGUAGE RankNTypes #-}
-- |

module Fresh.Infer where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Map as Map
import Control.Monad.ST (runST)
import Data.STRef

import Fresh.Kind (Kind(..))
import Fresh.Type (TypeAST(..), TypeABT(..), TCon(..), SType(..), Infer,
                   TypeError(..), inLevel, generalize, resolve, unresolve, Level(..), Type, getKind,
                   Id(..), freshTVar, QualType(..), CompositeLabelName(..),
                   TypeVar(..),
                   freshRVar, FlatComposite(..), unflattenComposite, EVarName(..),
                   InferState(..), Expr(..), QType, emptyQual, Lit(..))
import Fresh.Unify (unify, varBind)

tyRec :: TypeAST t
tyRec = TyCon (TCon (Id "Rec") (KArrow Composite Star))

tySum :: TypeAST t
tySum = TyCon (TCon (Id "Sum") (KArrow Composite Star))

tyFunc :: TypeAST t
tyFunc = TyCon (TCon (Id "->") (KArrow Star (KArrow Star Star)))


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
    tvar <- freshTVar
    (edef', QualType edefP edefT) <- withVar var tvar $ infer edef
    unify (SType $ TyVar tvar) edefT
    (expr', QualType exprP exprT) <- withVar var tvar $ infer expr
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

