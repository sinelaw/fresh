module Fresh.Unify where

import Control.Monad (forM_, when, void)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.STRef



import Fresh.Pretty (Pretty(..))
import Fresh.Type

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
unify' (SType (TyVar tvar1)) (SType (TyVar tvar2)) = do
    vt1 <- readVar tvar1
    vt2 <- readVar tvar2
    case (vt1, vt2) of
        (Unbound _n1 l1, Unbound _n2 l2) ->
            if l1 < l2
            then writeVar tvar2 vt1
            else writeVar tvar1 vt2
        (Unbound{}, Link lt2) -> varBind tvar1 lt2
        (Link lt1, Unbound{}) -> varBind tvar2 lt1
        (Link lt1, Link lt2) -> unify' lt1 lt2

unify' (SType (TyVar tvar)) t = varBind tvar t
unify' t (SType (TyVar tvar)) = varBind tvar t
unify' (SType (TyAST t1)) (SType (TyAST t2)) = unifyAST t1 t2

unifyAST :: TypeAST Level (SType s) -> TypeAST Level (SType s) -> Infer s ()
unifyAST (TyAp t1 t2) (TyAp t1' t2') = do
    unify t1 t1'
    unify t2 t2'
unifyAST (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
unifyAST (TyGenVar g1) (TyGenVar g2) | g1 == g2 = return ()
unifyAST u1@(TyGen vs1 (QualType ps1 t1)) u2@(TyGen vs2 (QualType ps2 t2)) | length vs1 == length vs2 = do
    -- TODO: check instance relation (subsumption)
    ks1 <- mapM getKind vs1
    ks2 <- mapM getKind vs2
    forM_ (zip ks1 ks2) $ \(k1, k2) -> when (k1 /= k2 ) $ throwError $ KindMismatchError k1 k2
    curLevel <- getCurrentLevel
    skolems <- mapM (\k -> GenVar <$> freshName <*> pure k <*> pure curLevel) ks1
    let skolemTs = map (SType . TyAST . TyGenVar) skolems
    t1' <- substGens vs1 skolemTs t1
    t2' <- substGens vs2 skolemTs t2
    unify t1' t2'
    gvs1 <- liftST $ freeGenVars u1
    gvs2 <- liftST $ freeGenVars u2
    when (not . Set.null $ Set.fromList skolems `Set.intersection` (gvs1 `Set.union` gvs2) )
        $ throwError
        $ EscapedSkolemError
        $ concat
        [ "Type not polymorphic enough to unify"
        , "\n\t", "Type 1: ", show u1
        , "\n\t", "Type 2: ", show u2
        ]

unifyAST (TyComp CompositeTerminal) (TyComp CompositeTerminal) = return ()

unifyAST (TyComp c1) (TyComp c2) = do
    let FlatComposite labels1 mEnd1 = flattenComposite c1
        FlatComposite labels2 mEnd2 = flattenComposite c2
        common = Map.intersectionWith (,) labels1 labels2
        in1only = Map.difference labels1 labels2
        in2only = Map.difference labels2 labels1
        emptyRow = SType $ TyAST $ TyComp CompositeTerminal
    -- TODO: wrap errors to say which field failed
    forM_ (Map.elems common) $ uncurry unify
    remainderVar <- freshRVar
    if Map.null in1only && Map.null in2only
        then case (mEnd1, mEnd2) of
                 (Nothing, Nothing) -> return ()
                 (Just e, Nothing) -> unify e emptyRow
                 (Nothing, Just e) -> unify e emptyRow
                 (Just e1, Just e2) -> unify e1 e2
        else do
            let remainderVarT = SType $ TyVar remainderVar
                unifyRemainder rem' mEnd = do
                    -- e1 + r1 = e2 + r2
                    -- (r + r2) + r1 = (r + r1) + r2
                    case mEnd of
                        Nothing ->
                            if Map.null rem'
                            then varBind remainderVar emptyRow
                            else traverse purify rem' >>= \pt -> throwError $ RowEndError (show $ fmap pretty pt)
                        Just end ->
                            unify (SType $ TyAST $ TyComp $ unflattenComposite $ FlatComposite rem' $ Just remainderVarT) end
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
        Link t' -> unify t' t
        Unbound name l1 -> --writeVar tvar (Link t)
            case t of
                (SType (TyVar tvar2)) -> do
                    vt2 <- readVar tvar2
                    case vt2 of
                        Link t2 -> unify (SType $ TyVar tvar) t2
                        Unbound _name2 l2 -> do
                            writeVar tvar (Link t)
                            -- adjust the lambda-rank of the unifiable variable
                            when (l2 > l1) (writeVar tvar2 (Unbound _name2 l1))
                (SType (TyAST tast)) -> do
                    tvs <- liftST $ freeVars tast
                    when (name `Set.member` tvs) $ do
                        pt <- purify t
                        throwError $ OccursError (show $ pretty vt) (show $ pretty pt)
                    writeVar tvar (Link t)
                    -- adjust the lambda-rank of the unifiable variables in tp2
                    adjustLevel l1 t

adjustLevel :: Level -> SType s -> Infer s ()
adjustLevel l (SType (TyVar tvar)) = do
    tv <- readVar tvar
    case tv of
        Link t -> adjustLevel l t
        Unbound name l' -> when (l' > l) (writeVar tvar (Unbound name l))
adjustLevel l (SType (TyAST t)) = void $ traverse (adjustLevel l) t
