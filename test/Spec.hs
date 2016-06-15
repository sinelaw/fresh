{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--module Spec where

import           Test.QuickCheck

import           Data.DeriveTH

import Data.Functor.Identity (runIdentity)
import           Control.Monad   (void, forM, forM_, when)
import Data.String (IsString(..))
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Fresh.Pretty ()
import Fresh.Kind (Kind(..))
import Fresh.Type (ETypeAsc(..), EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..), GenVar(..), Class(..), TypeError(..), getAnnotation, Composite(..), CompositeLabelName(..), FlatComposite(..), HasKind(..), Level(..), TypeError, tyFunc, tyRec)
import Fresh.Infer (inferExpr, runInfer, instantiateAnnot, qresolve, equivalent, equivalentQual, equivalentPred, subsume, skolemize)
import Fresh.Unify (unify)
import qualified Fresh.Type as Type
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), vsep, indent, (<+>), (<$$>), red)

instance IsString EVarName where
    fromString = EVarName

instance IsString CompositeLabelName where
    fromString = CompositeLabelName


eithers :: Eq a => (b -> b -> Bool) -> Either a b -> Either a b -> Bool
eithers f (Right r1) (Right r2) = f r1 r2
eithers _ (Left e1) (Left e2) = e1 == e2
eithers _ _ _ = False

forgetLeft :: Either l r -> Either () r
forgetLeft (Right x) = Right x
forgetLeft (Left _) = Left ()

fromRight :: Either l r -> r
fromRight (Right x) = x
fromRight (Left _) = error "fromRight!"

constWrap :: Expr () -> Expr ()
constWrap expr = let_ dummy (num 0) expr
    where dummy = "a very long name that won't be generated by arbitrary"


testEquivTypes :: Either l (QualType Type) -> Either l' (QualType Type) -> Bool
testEquivTypes inferred expected =
    eithers equivalentQual (forgetLeft inferred) (forgetLeft expected)


let_ :: EVarName -> Expr () -> Expr () -> Expr ()
let_ = ELet ()

var :: EVarName -> Expr ()
var = EVar ()

num :: Double -> Expr ()
num = ELit () . LitNum

infixr 5 ~$
(~$) :: Expr () -> Expr () -> Expr ()
(~$) = EApp ()

infixr 2 ~::
(~::) :: Expr () -> QualType Type -> Expr ()
(~::) = \expr qual -> EAsc () (ETypeAsc qual) expr

infixr 4 ~>
(~>) :: EVarName -> Expr () -> Expr ()
(~>) = ELam ()

infixr 5 ##
(##) :: Expr () -> CompositeLabelName -> Expr ()
(##) = EGetField ()


lama :: EVarName -> QualType Type -> Expr () -> Expr ()
lama v t = EALam () v (ETypeAsc t)

-- Types

tcon :: String -> Type
tcon x = Fix $ TyCon $ TCon (Id x) Star

_String :: Type
_String = tcon "String"

_Bool :: Type
_Bool = tcon "Bool"

_Number :: Type
_Number =  tcon "Number"

_Func :: Type
_Func = Fix tyFunc

(~=>) :: [Pred t] -> t -> QualType t
(~=>) = QualType

(^$) :: Type -> Type -> Type
f ^$ x = Fix $ TyAp f x

infixr 5 ^->
(^->) :: Type -> Type -> Type
targ ^-> tres = Fix $ TyAp (Fix $ TyAp _Func targ) tres

forall :: GenVar () -> Type -> Type
forall gv t = foralls [gv] t

forallsQ :: [Pred Type] -> [GenVar ()] -> Type -> Type
forallsQ ps gvs t = Fix $ TyGen gvs (QualType ps t)

foralls :: [GenVar ()] -> Type -> Type
foralls gvs t = forallsQ [] gvs t

gv :: Int -> GenVar ()
gv x = GenVar x Star ()

tv :: Int -> Type
tv x = Fix $ TyGenVar $ gv x

a, b, c, d, e, f, g :: Type
[a, b, c, d, e, f, g] = map tv [0,1,2,3,4,5,6]
a',b',c',d',e',f',g' :: GenVar ()
[a',b',c',d',e',f',g'] = map gv [0,1,2,3,4,5,6]

rv :: Int -> GenVar ()
rv x = GenVar x Composite ()
rtv :: Int -> Type
rtv x = Fix $ TyGenVar $ rv x

ra, rb, rc, rd, re, rf, rg :: Type
[ra, rb, rc, rd, re, rf, rg] = map rtv [0,1,2,3,4,5,6]
ra',rb',rc',rd',re',rf',rg' :: GenVar ()
[ra',rb',rc',rd',re',rf',rg'] = map rv [0,1,2,3,4,5,6]


record :: [(CompositeLabelName, Type)] -> Maybe Type -> Type
record fs rest = Fix tyRec ^$ (Fix $ TyComp c)
    where
        c = Type.unflattenComposite (FlatComposite (Map.fromList fs) rest)

-- Tests

wrapFooLet :: Expr () -> Expr ()
wrapFooLet x = let_ "foo" x $ var "foo"

exampleApIdNum :: Expr ()
exampleApIdNum = ("x" ~> var "x") ~$ num 2

testClass :: Class
testClass = Class (Id "TestClass") Star

idFunction :: Expr ()
idFunction = let_ "id" ("x" ~> var "x") $ var "id"

idBool :: Expr ()
idBool = lama "x" ([] ~=> _Bool) (var "x")

polyId :: Expr ()
polyId = lama "x" ([] ~=> forall (a') (a ^-> a)) (var "x")

examples :: [(Expr (), Either () (QualType Type))]
examples = [ ( ELit () (LitBool False) , Right $ [] ~=> _Bool)
           , ( var "x", Left () )
           , ( ("x" ~> (var "x")) ~:: ([] ~=> ((forall (a') (a ^-> a)) ^-> (forall (a') (a ^-> a))))
             , Right $ [] ~=> forall (b') ((forall (a') (a ^-> a)) ^-> (b ^-> b)))

           , ( idFunction              , Right $ [] ~=> forall (c') (c ^-> c))
           , ( idBool                  , Right $ [] ~=> (_Bool ^-> _Bool))
           , ( exampleApIdNum          , Right $ [] ~=> _Number)
           , ( exampleApIdNum ~:: ([] ~=> _Bool), Left ())
           , ( exampleApIdNum ~:: ([] ~=> _Number), Right $ [] ~=> _Number)
             -- TODO deal with alpha equivalence, preferrably by
             -- making generalization produce ids like GHC

           , ( let_ "id" ("x" ~> (var "x" ~:: ([] ~=> _Number))) $ var "id",
               Right $ [] ~=> (_Number ^-> _Number))

           , ( let_ "id" (lama "x" ([] ~=> forall (a') (a ^-> a)) (var "x")) $ var "id"
             , Right $ [] ~=> forall (e') ((forall (b') (b ^-> b)) ^-> (e ^-> e)))

           , ( (let_ "id" ("x" ~> (var "x")) (var "id"))  ~:: ([] ~=> ((forall (a') (a ^-> a)) ^-> (forall (a') (a ^-> a))))
             , Right $ [] ~=> ((forall (a') (a ^-> a)) ^-> (forall (a') (a ^-> a))))

           , ( let_ "id" ("x" ~> (var "x" ~:: ([] ~=> forall (d') (d ^-> d)))) $ var "id",
               Left ()) -- impredicative instantiation (eta-expansion of polymorphic arguments doens't work)

           , ( idFunction ~:: ([] ~=> forall (b') (b ^-> b)),
               Right $ [] ~=> forall (b') (b ^-> b))

           , ( idFunction ~:: ([] ~=> forallsQ [PredIs testClass $ b] [b'] (b ^-> b)),
               Right $ [] ~=> forallsQ [PredIs testClass $ b] [b'] (b ^-> b))

           , ( wrapFooLet ("y" ~> let_ "id" ("x" ~> var "y") (var "id"))
             , Right $ [] ~=> forall b' (forall d' (b ^-> d ^-> b)))

           , ( wrapFooLet ("y" ~> let_ "id" ("x" ~> var "y") (var "id"))
             , Right $ [] ~=> forall b' (forall d' (d ^-> b ^-> d)))

           , ( wrapFooLet ("y" ~> let_ "id" ("x" ~> var "y") (var "id"))
             , Right $ [] ~=> forall b' (forall a' (a ^-> b ^-> a)))

           , ( wrapFooLet ("y" ~> let_ "id" ("x" ~> var "y") (var "id"))
             , Right $ [] ~=> forall a' (forall b' (a ^-> b ^-> a)))

           , ( let_ "zero" ("x" ~> var "x" ~$ num 0) (var "zero")
             , Right $ [] ~=> forall (e') ((_Number ^-> e) ^-> e))

           , ( wrapFooLet ("x" ~> "y" ~> var "x")
             , Right $ [] ~=> foralls [f', g'] (f ^-> g ^-> f))

           , ( ("x" ~> var "x" ## "fieldName")
             , Right $ [] ~=> foralls [d', re'] (record [("fieldName", d)] (Just $ re) ^-> d))

           , ( let_ "id"
               ("x" ~>
                (((var "x") ## "fieldName") ~:: [] ~=> _Number))
               $ var "id"
             , Right $ [] ~=> foralls [rf'] (record [("fieldName", _Number)] (Just $ rf) ^-> _Number))

           , ( EGetField () (ELet () (EVarName "r") (EApp () (EGetField () (EVar () (EVarName "r")) (CompositeLabelName "pbe")) (ELam () (EVarName "x") (EVar () (EVarName "x")))) (EVar () (EVarName "r"))) (CompositeLabelName "nid")
             , Left () ) -- occurs

           , ( lama "a" ([PredIs (Class (Id "C") Star) e] ~=> e) ("b" ~>  (ELit () (LitString "c")))
             , Right $ [] ~=> forall d' ((forallsQ [PredIs (Class (Id "C") Star) e] [e'] e) ^-> (d ^-> _String)) )

           , ( lama "a" ([PredIs (Class (Id "C") Star) e] ~=> (forall f' f)) ("b" ~>  (ELit () (LitString "c")))
             , Right $ [] ~=> forall d' ((forallsQ [PredIs (Class (Id "C") Star) e] [e', f'] f) ^-> (d ^-> _String)) )
           ]

-- ----------------------------------------------------------------------

isRight :: Either a b -> Bool
isRight Right{} = True
isRight Left{} = False


-- instance Arbitrary (t (Fix t)) => Arbitrary (Fix t) where
--     arbitrary = Fix <$> arbitrary

instance Arbitrary g => Arbitrary (GenVar g) where
    arbitrary = GenVar <$> (getPositive <$> arbitrary) <*> arbitrary <*> arbitrary
    shrink (GenVar i k x) = GenVar <$> shrink i <*> shrink k <*> shrink x

derive makeArbitrary ''Level

instance Arbitrary Id where
    arbitrary = oneof (map (pure . Id) ["A", "B", "C", "D", "E", "F"])

instance Arbitrary Kind where
    arbitrary = oneof (map pure
                       [ Star
                       , Composite
                       , KArrow Star Star
                       , KArrow Composite Star
                       , KArrow (KArrow Star Star) Star
                       ])
    shrink (KArrow t1 t2) = [t1, t2]
    shrink _ = []

derive makeArbitrary ''TCon

instance Arbitrary CompositeLabelName where
    arbitrary = CompositeLabelName <$> (take 5 <$> shuffle ['a'..'z'] >>= sublistOf)

derive makeArbitrary ''Composite

genTyAp :: Gen Type
genTyAp = do
    tf <- suchThat arbitrary $
          \t -> case kind t of
                    Just KArrow{} -> True
                    _ -> False
    let (Just (KArrow kx _)) = kind tf
    tx <- (Fix . TyCon) <$> (TCon <$> arbitrary <*> pure kx)
    let res = Fix $ TyAp tf tx
    -- assertion:
    when (not . isJust $ kind res) $ error $ "Wat: " ++ show res
    return res

genTyCon :: Gen TCon
genTyCon = TCon <$> arbitrary <*> arbitrary

-- genPred :: GenVar () -> Gen (Pred Type)
-- genPred gv = PredIs <$> arbitrary <*> (pure $ Fix $ TyGenVar gv)

genTyGen :: Gen Type
genTyGen = do
    t <- arbitrary :: Gen Type
    gvSet <- Type.freeGenVars t
    case Set.toList gvSet of
        [] -> pure t
        gvs -> pure $ Fix $ TyGen gvs (QualType [] t)

instance Arbitrary Type where
    arbitrary = oneof $
        [ genTyAp
        , Fix . TyCon <$> genTyCon
        , Fix . TyGenVar <$> arbitrary
        , genTyGen
--        , Fix . TyComp <$> arbitrary
        ]

    shrink (Fix (TyAp t1 t2)) = [t1, t2]
    shrink (Fix TyCon{}) = []
    shrink (Fix TyGenVar{}) = []
    shrink (Fix (TyGen _ (QualType ps t))) = [t]
    shrink (Fix TyComp{}) = [] -- TODO

instance (Arbitrary t, HasKind t) => Arbitrary (Pred t) where
    arbitrary = oneof $
        [ do
                t <- arbitrary
                let Just k = kind t
                PredIs <$> (Class <$> arbitrary <*> pure k) <*> pure t
        , PredNoLabel <$> arbitrary <*> arbitrary
        ]
    shrink (PredIs c t) = PredIs <$> shrink c <*> shrink t
    shrink (PredNoLabel l t) = PredNoLabel <$> shrink l <*> shrink t

instance (Arbitrary t, HasKind t) => Arbitrary (QualType t) where
    arbitrary = QualType <$> arbitrary <*> arbitrary
    shrink (QualType ps t) = QualType <$> shrink ps <*> shrink t

derive makeArbitrary ''Class

derive makeArbitrary ''Lit
derive makeArbitrary ''EVarName
derive makeArbitrary ''ETypeAsc
derive makeArbitrary ''Expr

prop_ordLevel :: Level -> Bool
prop_ordLevel l = [l] == Set.toList (Set.singleton l `Set.intersection` Set.singleton l)

-- This just tests the Arbitrary instance for Type: it should only
-- generate valid types (ones that have a kind)
prop_hasKind :: Type -> Bool
prop_hasKind = isJust . kind


prop_resolve :: Type -> Bool
prop_resolve t =
    case (runInfer $ (Type.unresolve t >>= Type.resolve)) of
        Right (Just t') -> equivalent t t'
        _ -> False


-- TODO: Wrong
prop_skolemize :: Type -> Bool
prop_skolemize t =
    case getSkolemized t of
    Right (Just s) -> equivalent (wrapGen t) (wrapGen s)
    _ -> False
    where
        getSkolemized x = runInfer $ (Type.unresolve x) >>= skolemize  >>= ( Type.resolve . Type.qualType . snd)
        wrapGen ty = case Set.toList $ runIdentity $ Type.freeGenVars ty of
            [] -> ty
            gvs -> Fix $ TyGen gvs (QualType [] ty)

-- prop_hasKindStar :: Type -> Bool
-- prop_hasKindStar t = Just Star == kind t

prop_constExpand :: Expr () -> Bool
prop_constExpand expr =
    case (getAnnotation <$> inferExpr (constWrap expr), getAnnotation <$> inferExpr expr) of
        (Right cres, Right res)                     -> equivalentQual cres res
        -- Left (WrappedError _ (WrappedError _ e)) -> Left e == res
        -- _                                        -> error "Expected success or WrappedError on const-wrapped"
        (Left{}, Left{})                            -> True
        _                                           -> False

testSubsume :: Type -> Type -> Either TypeError ()
testSubsume t1 t2 = runInfer $ do
    t1' <- Type.unresolve t1
    t2' <- Type.unresolve t2
    subsume t1' t2'

prop_selfSubsume :: Type -> Bool
prop_selfSubsume t =
    case kind t of
        Just k -> isRight $ testSubsume t t
        _ -> error "Arbitrary Type must have kind."

prop_selfSubsumeNormalized :: Type -> Bool
prop_selfSubsumeNormalized t =
    case kind t of
        Just k -> isRight $ testSubsume t (Type.normalize t)
        _ -> error "Arbitrary Type must have kind."

prop_selfEquivalence :: Type -> Bool
prop_selfEquivalence t = equivalent t t

prop_selfEquivalenceNormalized :: Type -> Bool
prop_selfEquivalenceNormalized t = equivalent t (Type.normalize t)

prop_selfEquivalencePred :: Pred Type -> Bool
prop_selfEquivalencePred p = equivalentPred p p

prop_selfEquivalenceQual :: QualType Type -> Bool
prop_selfEquivalenceQual q = equivalentQual q q

testUnify :: Type -> Type -> Either TypeError ()
testUnify t1 t2 = runInfer $ do
    (QualType ps1 ut1) <- Type.unresolve t1 >>= Type.instantiate
    (QualType ps2 ut2) <- Type.unresolve t2 >>= Type.instantiate
    unify ut1 ut2

prop_unifySame :: Type -> Bool
prop_unifySame t =
    case kind t of
        Just Star -> Right () == testUnify t t
        _ -> True -- don't test

shouldUnify :: Bool -> Type -> Type -> IO ()
shouldUnify b t1 t2 = do
    putStrLn $ "Unifying: " ++ show (pretty t1) ++ " with " ++ show (pretty t2) ++ " - should succeed: " ++ show b
    let res = testUnify t1 t2
    when (b == (Right () /= res)) $ error $ "Wrong result: " ++ (show $ pretty res)

erecord :: [(CompositeLabelName, Type)] -> Type
erecord x = record x Nothing

rightPad :: a -> Int -> [a] -> [a]
rightPad ch n []
    | n > 0     = take n $ repeat ch
    | otherwise = []
rightPad ch n (x:xs)
    | n > 0     = x : rightPad ch (n-1) xs
    | otherwise = (x:xs)

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 5000 })

main :: IO ()
main = do
    putStrLn "Testing..."
    shouldUnify True  (erecord []) (erecord [])
    shouldUnify True  (erecord [("x", _Bool)]) (erecord [("x", _Bool)])
    shouldUnify True  (erecord [("x", _Bool)]) (forall re' $ record [] $ Just re)
    shouldUnify False (erecord [("x", _Bool)]) (erecord [("x", _Number)])
    shouldUnify False (erecord [("x", _Bool)]) (erecord [("y", _Bool)])

    -- TODO Fix
    -- shouldUnify True (record [("num", _Number)] Nothing) (record [] (Just ra))
    -- shouldUnify True (record [("num", _Number)] (Just ra)) (record [("bool", _Bool)] (Just rb))

    merrs <- forM examples $ \(x, t) -> do
        putStrLn "------------------------------------------------------------"
        putStr $ rightPad ' ' 40 $ show $ pretty x
        putStr " :: (inferred) "
        let inferredType = getAnnotation <$> inferExpr x
            conInferredType = getAnnotation <$> inferExpr (constWrap x)
            errMsgInfer = Just $ pretty $ vsep
                [ "TEST FAILED!"
                , "Wrong type inferred for:" <+> pretty x
                ] <$$>
                (indent 4 $ vsep
                [ "Expected:" <$$> (pretty t) -- , " = " , ( t) , "\n"
                , "Expected (normalized):" <$$> pretty (Type.normalizeQual <$> t)
                , "Inferred:" <$$> (pretty inferredType) -- , " = " , ( inferredType)
                , "Inferred (normalized):" <$$> pretty (Type.normalizeQual <$> inferredType)
                , "Inferred (raw): " <$$> pretty inferredType
                , "Constwrap-Inferred:" <$$> (pretty conInferredType) -- , " = " , (show inferredType) , "\n"
                ])
        print . pretty $ inferredType
        if (not $ testEquivTypes inferredType conInferredType)
            then return $ Just $ pretty $ vsep
                 [ "TEST FAILED!"
                 , "Type not equivalent to constwrap of itself:"
                 , pretty inferredType
                 , pretty conInferredType
                 ]
            else if ((testEquivTypes inferredType t) && (testEquivTypes conInferredType t))
                 then return Nothing
                 else return errMsgInfer

    let errs = catMaybes merrs
    when (not $ null errs) $
        forM_ errs (putStrLn . show . red)
    putStrLn "------------------------------------------------------------"
    void runTests


-- TODO: Check this example, it fails constWrap and also infers a type
-- that shadows some genvar (e) in the ETypeAsc:
-- let t = EALam () (EVarName "a") (ETypeAsc (QualType {qualPred = [PredIs (Class (Id "C") Star) (Fix {unFix = TyGenVar {_tyGenVar = GenVar {genVarId = 4, genVarKind = Star, genVarAnnot = ()}}})],
--                                                      qualType = Fix {unFix = TyGenVar {_tyGenVar = GenVar {genVarId = 5, genVarKind = Star, genVarAnnot = ()}}}
--                                  }))
--                                  (ELam () (EVarName "b") (ELit () (LitString "c")))
-- lama "a" ([PredIs (Class (Id "C") Star) e'] ~=> ("b" ~>  (ELit () (LitString "c"))))
