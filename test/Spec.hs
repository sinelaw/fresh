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
import Fresh.Type (inferExpr, EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..), GenVar(..), Class(..), TypeError(..), getAnnotation, Composite(..), CompositeLabelName(..), FlatComposite(..), HasKind(..))
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

(~$) :: Expr () -> Expr () -> Expr ()
(~$) = EApp ()

(~::) :: Expr () -> QualType Type -> Expr ()
(~::) = flip $ EAsc ()

(~>) :: EVarName -> Expr () -> Expr ()
(~>) = ELam ()

(##) = EGetField ()

-- Types

tcon :: String -> Type
tcon x = Fix $ TyCon $ TCon (Id x) Star

_Bool :: Type
_Bool = tcon "Bool"

_Number :: Type
_Number =  tcon "Number"

_Func :: Type
_Func = Fix Type.tyFunc

(~=>) :: [Pred t] -> t -> QualType t
(~=>) = QualType

(^$) :: Type -> Type -> Type
f ^$ x = Fix $ TyAp f x

infixr 5 ^->
(^->) :: Type -> Type -> Type
targ ^-> tres = Fix $ TyAp (Fix $ TyAp _Func targ) tres

forall :: [GenVar] -> Type -> Type
forall gvs t = Fix $ TyGen gvs t

gv :: Int -> GenVar
gv x = GenVar x Star

tv :: Int -> Type
tv x = Fix $ TyGenVar $ gv x

a, b, c, d, e :: Type
[a, b, c, d, e] = map tv [0,1,2,3,4]
a',b',c',d',e' :: GenVar
[a',b',c',d',e'] = map gv [0,1,2,3,4]

record :: [(CompositeLabelName, Type)] -> Maybe Type -> Type
record fs rest = (Fix Type.tyRec) ^$ (Fix $ TyComp c)
    where
        c = Type.unflattenComposite (FlatComposite (Map.fromList fs) rest)

-- Tests

wrapFooLet :: Expr () -> Expr ()
wrapFooLet x = let_ "foo" x $ var "foo"

exampleApIdNum :: Expr ()
exampleApIdNum = "x" ~> (var "x") ~$ num 2

examples :: [(Expr (), Either TypeError (QualType Type))]
examples = [ (exampleApIdNum,                      Right $ [] ~=> _Number)
           , (exampleApIdNum ~:: ([] ~=> _Bool),   Left Type.UnificationError)
           , (exampleApIdNum ~:: ([] ~=> _Number), Right $ [] ~=> _Number)
           , (ELit () (LitBool False),             Right $ [] ~=> _Bool)
             -- TODO deal with alpha equivalence, preferrably by
             -- making generalization produce ids like GHC
           , (let_ "id" ("x" ~> var "x") $ var "id", Right $ [] ~=> (forall [b'] $ b ^-> b))

           , ( wrapFooLet ("y" ~> (let_ "id" ("x" ~> var "y") $ var "id"))
             , Right $ [] ~=> (forall [b', d'] $ b ^-> d ^-> b))

           , ( wrapFooLet ("y" ~> ("x" ~> var "y"))
             , Right $ [] ~=> (forall [b', c'] $ b ^-> c ^-> b))

           , (let_ "id" ("x" ~> ((var "x") ## "fieldName")) $ var "id",
              Right $ [] ~=> (forall [c', d'] $ (record [("fieldName", c)] $ Just d) ^-> c))
           ]

-- ----------------------------------------------------------------------

instance Arbitrary (t (Fix t)) => Arbitrary (Fix t) where
    arbitrary = Fix <$> arbitrary

instance Arbitrary GenVar where
    arbitrary = GenVar <$> (getPositive <$> arbitrary) <*> arbitrary

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
constWrap expr = (("x" ~> expr) ~$ num 0)

-- prop_constExpand :: Expr () -> Bool
-- prop_constExpand expr = (getAnnotation <$> inferExpr expr) == (getAnnotation <$> inferExpr (constWrap expr))

testUnify :: Type -> Type -> Either TypeError ()
testUnify t1 t2 = Type.runInfer $ Type.unify (Type.unresolve t1) (Type.unresolve t2)

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
        putStrLn . show . pretty $ inferredType
        when (inferredType /= t)
            $ error
            $ "Wrong type."
            ++ "\t" ++ "Expected: " ++ (show $ pretty t) -- ++ " = " ++ (show t) ++ "\n"
            ++ "\t" ++ "Inferred: " ++ (show $ pretty inferredType) -- ++ " = " ++ (show inferredType) ++ "\n"
        -- print . show $ getAnnotation <$> inferExpr x
        -- print . show $ getAnnotation <$> inferExpr (constWrap x)
        putStrLn "------------------------------------------------------------"
    void runTests


