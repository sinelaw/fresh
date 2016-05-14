{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Test.QuickCheck

import           Data.DeriveTH

import           Control.Monad   (void, forM_, when)
import Data.String (IsString(..))
import qualified Data.Map as Map
import Fresh.Pretty ()
import Fresh.Kind (Kind(..))
import Fresh.Type (ETypeAsc(..), EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..), GenVar(..), Class(..), TypeError(..), getAnnotation, Composite(..), CompositeLabelName(..), FlatComposite(..), HasKind(..), Level(..), TypeError, tyFunc, tyRec)
import Fresh.Infer (inferExpr, runInfer)
import Fresh.Unify (unify)
import qualified Fresh.Type as Type
import Text.PrettyPrint.ANSI.Leijen (Pretty(..))

instance IsString EVarName where
    fromString = EVarName

instance IsString CompositeLabelName where
    fromString = CompositeLabelName

-- Example:

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

forall :: GenVar -> Type -> Type
forall gv t = foralls [gv] t

foralls :: [GenVar] -> Type -> Type
foralls gvs t = Fix $ TyGen gvs t

gv :: Int -> GenVar
gv x = GenVar x Star LevelAny

tv :: Int -> Type
tv x = Fix $ TyGenVar $ gv x

a, b, c, d, e, f, g :: Type
[a, b, c, d, e, f, g] = map tv [0,1,2,3,4,5,6]
a',b',c',d',e',f',g' :: GenVar
[a',b',c',d',e',f',g'] = map gv [0,1,2,3,4,5,6]

rv :: Int -> GenVar
rv x = GenVar x Composite LevelAny
rtv :: Int -> Type
rtv x = Fix $ TyGenVar $ rv x

ra, rb, rc, rd, re, rf, rg :: Type
[ra, rb, rc, rd, re, rf, rg] = map rtv [0,1,2,3,4,5,6]
ra',rb',rc',rd',re',rf',rg' :: GenVar
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

idFunction = let_ "id" ("x" ~> var "x") $ var "id"
idBool = lama "x" ([] ~=> _Bool) (var "x")
polyId = lama "x" ([] ~=> forall (a') (a ^-> a)) (var "x")

examples :: [(Expr (), Either () (QualType Type))]
examples = [ ( ELit () (LitBool False) , Right $ [] ~=> _Bool)
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

           -- , ( idFunction ~:: ([] ~=> forall (b') (b ^-> b)),
           --     Right $ [] ~=> forall (b') (b ^-> b))

           -- , ( idFunction ~:: ([PredIs testClass $ b] ~=> forall (b') (b ^-> b)),
           --     Right $ [PredIs testClass $ b] ~=> forall (b') (b ^-> b))

           -- , ( wrapFooLet ("y" ~> let_ "id" ("x" ~> var "y") (var "id"))
           --   , Right $ [] ~=> forall b' (forall d' (b ^-> d ^-> b)))

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
           ]

-- ----------------------------------------------------------------------

instance Arbitrary (t (Fix t)) => Arbitrary (Fix t) where
    arbitrary = Fix <$> arbitrary

instance Arbitrary GenVar where
    arbitrary = GenVar <$> (getPositive <$> arbitrary) <*> arbitrary <*> arbitrary

derive makeArbitrary ''Level
derive makeArbitrary ''Id
derive makeArbitrary ''Kind
derive makeArbitrary ''TCon
derive makeArbitrary ''CompositeLabelName
derive makeArbitrary ''Composite
derive makeArbitrary ''TypeAST
derive makeArbitrary ''Pred
derive makeArbitrary ''QualType
derive makeArbitrary ''Class

derive makeArbitrary ''Lit
derive makeArbitrary ''EVarName
derive makeArbitrary ''ETypeAsc
derive makeArbitrary ''Expr

constWrap :: Expr () -> Expr ()
constWrap expr = let_ dummy (num 0) expr
    where dummy = "a very long name that won't be generated by arbitrary"

prop_constExpand :: Expr () -> Bool
prop_constExpand expr =
    case (getAnnotation <$> inferExpr (constWrap expr), getAnnotation <$> inferExpr expr) of
        (Right cres, Right res)                     -> cres == res
        -- Left (WrappedError _ (WrappedError _ e)) -> Left e == res
        -- _                                        -> error "Expected success or WrappedError on const-wrapped"
        (Left{}, Left{})                            -> True
        _                                           -> False

testUnify :: Type -> Type -> Either TypeError ()
testUnify t1 t2 = runInfer $ unify (Type.unresolve t1) (Type.unresolve t2)

prop_unifySame :: Type -> Bool
prop_unifySame t =
    case kind t of
        Just Star -> Right () == testUnify t t
        _ -> True -- don't test

return []

runTests :: IO Bool
runTests = $quickCheckAll


shouldUnify b t1 t2 = do
    putStrLn $ "Unifying: " ++ show (pretty t1) ++ " with " ++ show (pretty t2) ++ " - should succeed: " ++ show b
    when (b == (Right () /= testUnify t1 t2)) $ error "Unify failed"

erecord x = record x Nothing

rightPad c n []
    | n > 0     = take n $ repeat c
    | otherwise = []
rightPad c n (x:xs)
    | n > 0     = x : rightPad c (n-1) xs
    | otherwise = (x:xs)

forgetLeft (Right x) = Right x
forgetLeft (Left _) = Left ()

main :: IO ()
main = do
    putStrLn "Testing..."
    shouldUnify True  (erecord []) (erecord [])
    shouldUnify True  (erecord [("x", _Bool)]) (erecord [("x", _Bool)])
    shouldUnify False (erecord [("x", _Bool)]) (erecord [("x", _Number)])
    shouldUnify False (erecord [("x", _Bool)]) (erecord [("y", _Bool)])

    forM_ examples $ \(x, t) -> do
        putStrLn "------------------------------------------------------------"
        putStr $ rightPad ' ' 40 $ show $ pretty x
        putStr " :: "
        let inferredType = getAnnotation <$> inferExpr x
        print . pretty $ inferredType
        when (forgetLeft inferredType /= t)
            $ error
            $ concat
            [ "Wrong type."
            , "\n"
            , "\t" , "Expected: " , show (pretty t) -- , " = " , (show t) , "\n"
            , "\n"
            , "\t" , "Inferred: " , show (pretty inferredType) -- , " = " , (show inferredType) , "\n"
            -- , "\n"
            -- , "\t" , "Raw Expected: " , show t
            -- , "\n"
            -- , "\t" , "Raw Inferred: " , show inferredType
            ]
        -- print . show $ getAnnotation <$> inferExpr x
        -- print . show $ getAnnotation <$> inferExpr (constWrap x)
    putStrLn "------------------------------------------------------------"
    void runTests


