module Fresh.Unify where

import Control.Monad (forM_, when)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.STRef


import Fresh.Pretty (Pretty(..))
import Fresh.Type (SType(..), TypeAST(..), TypeABT(..), Infer, TypeError(..),
                   GenVar(..), TypeVar(..),
                   freshName, freshRVar, purify,
                   getCurrentLevel, substGen, liftST, HasGen(..),
                   FlatComposite(..), flattenComposite, unflattenComposite,
                   getKind, readVar, writeVar, TVarLink(..))

unchain :: SType s -> Infer s (SType s)
unchain t@(SType (TyVar tvar)) = do
    vt <- readVar tvar
    case vt of
        Unbound{} -> return t
        Link t' -> unchain t'
unchain t = return t

unify :: SType s -> SType s -> Infer s ()
unify t1 t2 = do
    k1 <- getKind t1
    k2 <- getKind t2
    when (k1 /= k2) $ throwError $ KindMismatchError k1 k2
    t1' <- unchain t1
    t2' <- unchain t2
    pt1 <- purify t1'
    pt2 <- purify t2'
    let wrapError :: TypeError -> Infer s ()
        wrapError e = throwError (WrappedError (UnificationError (show $ pretty pt1) (show $ pretty pt2)) e)
    unify' t1' t2' `catchError` wrapError

unify' :: SType s -> SType s -> Infer s ()
unify' (SType (TyVar tvar1)) t2@(SType (TyVar tvar2)) = do
    vt1 <- readVar tvar1
    vt2 <- readVar tvar2
    case (vt1, vt2) of
        (Unbound _n1 l1, Unbound _n2 l2) ->
            if l1 < l2
            then writeVar tvar2 vt1
            else writeVar tvar1 vt2
        _ -> varBind tvar1 t2 -- TODO

unify' (SType (TyVar tvar)) t = varBind tvar t
unify' t (SType (TyVar tvar)) = varBind tvar t
unify' (SType (TyAST t1)) (SType (TyAST t2)) = unifyAST t1 t2

unifyAST :: TypeAST (SType s) -> TypeAST (SType s) -> Infer s ()
unifyAST (TyAp t1 t2) (TyAp t1' t2') = do
    unify t1 t1'
    unify t2 t2'
unifyAST (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
unifyAST (TyGenVar g1) (TyGenVar g2) | g1 == g2 = return ()
unifyAST u1@(TyGen v1 t1) u2@(TyGen v2 t2) = do
    -- TODO: check instance relation (subsumption)
    k1 <- getKind v1
    k2 <- getKind v2
    when (k1 /= k2) $ throwError $ KindMismatchError k1 k2
    skolem <- GenVar <$> freshName <*> pure k1 <*> getCurrentLevel
    let skolemT = SType . TyAST $ TyGenVar skolem
    t1' <- substGen v1 skolemT t1
    t2' <- substGen v2 skolemT t2
    unify t1' t2'
    gvs1 <- liftST $ freeGenVars u1
    gvs2 <- liftST $ freeGenVars u2
    when (Set.member skolem (gvs1 `Set.union` gvs2) )
        $ throwError
        $ EscapedSkolemError
        $ concat
        [ "Type not polymorphic enough to unify"
        , "\n\t", "Type 1: ", show u1
        , "\n\t", "Type 2: ", show u2
        ]

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
        unifyRemainder rem' mEnd =
            if Map.null rem'
            then case mEnd of
                Nothing -> return ()
                Just t -> purify t >>= \pt -> throwError $ RowEndError (show $ pretty pt) -- TODO really?
            else case mEnd of
                Nothing -> throwError $ RowEndError (show rem')
                Just end -> unifyAST (TyComp $ unflattenComposite $ FlatComposite rem' $ Just remainderVarT) $ fromEnd end
    unifyRemainder in1only mEnd2
    unifyRemainder in2only mEnd1

unifyAST t1 t2 = unifyError (SType $ TyAST t1) (SType $ TyAST t2)

unifyError :: SType s -> SType s -> Infer s a
unifyError t1 t2 = do
    pt1 <- purify t1
    pt2 <- purify t2
    throwError $ UnificationError (show $ pretty pt1) (show $ pretty pt2)


varBind :: TypeVar (STRef s) (SType s) -> SType s -> Infer s ()
varBind tvar t = do
    tvarK <- getKind tvar
    tK <- getKind t
    when (tvarK /= tK) $ throwError $ KindMismatchError tvarK tK
    vt <- readVar tvar
    case vt of
        Unbound _name l -> writeVar tvar (Link t)
        Link t' -> unify t' t -- TODO occurs
