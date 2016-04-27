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
import Fresh.Type (EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..), GenVar(..), Class(..), TypeError(..), getAnnotation, Composite(..), CompositeLabelName(..), FlatComposite(..), HasKind(..), Level(..), TypeError)
import Fresh.Infer (inferExpr, tyFunc, tyRec, runInfer)
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

-- infixr 2 ~::
-- (~::) :: Expr () -> QualType Type -> Expr ()
-- (~::) = flip $ EAsc ()

infixr 4 ~>
(~>) :: EVarName -> Expr () -> Expr ()
(~>) = ELam ()

infixr 5 ##
(##) :: Expr () -> CompositeLabelName -> Expr ()
(##) = EGetField ()

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

gv :: Int -> Int -> GenVar
gv x l = GenVar x Star (Level l)

tv :: Int -> Int -> Type
tv x l = Fix $ TyGenVar $ gv x l

a, b, c, d, e, f, g :: Int -> Type
[a, b, c, d, e, f, g] = map tv [0,1,2,3,4,5,6]
a',b',c',d',e',f',g' :: Int -> GenVar
[a',b',c',d',e',f',g'] = map gv [0,1,2,3,4,5,6]

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

examples :: [(Expr (), Either TypeError (QualType Type))]
examples = [ ( ELit () (LitBool False),             Right $ [] ~=> _Bool)
           , ( idFunction, Right $ [] ~=> forall (c' 0) (c 0 ^-> c 0))
           , ( exampleApIdNum,                      Right $ [] ~=> _Number)
           -- , ( exampleApIdNum ~:: ([] ~=> _Bool),   Left Type.UnificationError)
           -- , ( exampleApIdNum ~:: ([] ~=> _Number), Right $ [] ~=> _Number)
             -- TODO deal with alpha equivalence, preferrably by
             -- making generalization produce ids like GHC

           -- , ( let_ "id" ("x" ~> (var "x" ~:: ([] ~=> _Number))) $ var "id",
           --     Right $ [] ~=> (_Number ^-> _Number))

           -- , ( let_ "id" ("x" ~> (var "x" ~:: ([] ~=> forall (d' 0) (d 0 ^-> d 0)))) $ var "id",
           --     Right $ [] ~=> ((forall (d' 0) (d 0 ^-> d 0)) ^-> (forall (d' 0) (d 0 ^-> d 0))))

           -- , ( idFunction ~:: ([PredIs testClass $ b 0] ~=> forall (b' 0) (b 0 ^-> b 0)),
           --     Right $ [PredIs testClass $ b 0] ~=> forall (b' 0) (b 0 ^-> b 0))

           -- , ( wrapFooLet ("y" ~> let_ "id" ("x" ~> var "y") (var "id"))
           --   , Right $ [] ~=> forall b' (forall d' (b ^-> d ^-> b)))

           , ( let_ "zero" ("x" ~> var "x" ~$ num 0) (var "zero")
             , Right $ [] ~=> forall (e' 0) ((_Number ^-> e 0) ^-> e 0))

           , ( wrapFooLet ("x" ~> "y" ~> var "x")
             , Right $ [] ~=> foralls [f' 0, g' 0] (f 0 ^-> g 0 ^-> f 0))

           -- , ( let_ "id" ("x" ~> var "x" ## "fieldName") $ var "id"
           --   , Right $ [] ~=> forall c' (forall d' (record [("fieldName", c)] (Just d) ^-> c)))

           -- , ( let_ "id"
           --     ("x" ~>
           --      (((var "x") ## "fieldName") ~:: [] ~=> _Number))
           --     $ var "id"
           --   , Right $ [] ~=> forall d' (record [("fieldName", _Number)] (Just d) ^-> _Number))
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
derive makeArbitrary ''Expr

constWrap :: Expr () -> Expr ()
constWrap expr = (dummy ~> expr) ~$ num 0
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
        when (inferredType /= t)
            $ error
            $ concat
            [ "Wrong type."
            , "\n"
            , "\t" , "Expected: " , show (pretty t) -- , " = " , (show t) , "\n"
            , "\n"
            , "\t" , "Inferred: " , show (pretty inferredType) -- , " = " , (show inferredType) , "\n"
            ]
        -- print . show $ getAnnotation <$> inferExpr x
        -- print . show $ getAnnotation <$> inferExpr (constWrap x)
    putStrLn "------------------------------------------------------------"
    void runTests


