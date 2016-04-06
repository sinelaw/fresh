{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Test.QuickCheck

import           Data.DeriveTH

import           Control.Monad   (void, forM_, when)
import Data.String (IsString(..))
import Fresh.Pretty ()
import Fresh.Kind (Kind(..))
import Fresh.Type (inferExpr, EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..), GenVar(..), Class(..), TypeError(..), getAnnotation)
import qualified Fresh.Type as Type
import Text.PrettyPrint.ANSI.Leijen (Pretty(..))

instance IsString EVarName where
    fromString = EVarName


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

(^->) :: Type -> Type -> Type
targ ^-> tres = Fix $ TyAp (Fix $ TyAp _Func targ) tres

forall :: [Int] -> Type -> Type
forall gvs t = Fix $ TyGen (map gv gvs) t

gv :: Int -> GenVar
gv x = GenVar x Star

tv :: Int -> Type
tv x = Fix $ TyGenVar $ gv x

-- Tests

wrapFooLet :: Expr () -> Expr ()
wrapFooLet x = let_ "foo" x $ var "foo"

exampleApIdNum = "x" ~> (var "x") ~$ num 2

examples = [ (exampleApIdNum,                      Right $ [] ~=> _Number)
           , (exampleApIdNum ~:: ([] ~=> _Bool),   Left Type.UnificationError)
           , (exampleApIdNum ~:: ([] ~=> _Number), Right $ [] ~=> _Number)
           , (ELit () (LitBool False),             Right $ [] ~=> _Bool)
             -- TODO deal with alpha equivalence, preferrably by
             -- making generalization produce ids like GHC
           , (let_ "id" ("x" ~> var "x") $ var "id", Right $ [] ~=> (forall [1] $ (tv 1 ^-> tv 1)))

           , ( wrapFooLet ("y" ~> (let_ "id" ("x" ~> var "y") $ var "id"))
             , Right $ [] ~=> (forall [1, 3] $ (tv 1 ^-> (tv 3 ^-> tv 1))))

           , ( wrapFooLet ("y" ~> ("x" ~> var "y"))
             , Right $ [] ~=> (forall [1, 2] $ (tv 1 ^-> (tv 2 ^-> tv 1))))
           ]

-- ----------------------------------------------------------------------

instance Arbitrary (t (Fix t)) => Arbitrary (Fix t) where
    arbitrary = Fix <$> arbitrary

derive makeArbitrary ''GenVar
derive makeArbitrary ''Id
derive makeArbitrary ''Kind
derive makeArbitrary ''TCon
derive makeArbitrary ''TypeAST
derive makeArbitrary ''Pred
derive makeArbitrary ''QualType
derive makeArbitrary ''Class

derive makeArbitrary ''Lit
derive makeArbitrary ''EVarName
derive makeArbitrary ''Expr

constWrap expr = (("x" ~> expr) ~$ num 0)

prop_constExpand :: Expr () -> Bool
prop_constExpand expr = (getAnnotation <$> inferExpr expr) == (getAnnotation <$> inferExpr (constWrap expr))

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    forM_ examples $ \(x, t) -> do
        print $ pretty x
        let inferredType = getAnnotation <$> inferExpr x
        print . pretty $ inferredType
        when (inferredType /= t) $ error $ "Wrong type. Expected: " ++ (show $ pretty t) ++ " inferred: " ++ (show $ pretty inferredType)
        -- print . show $ getAnnotation <$> inferExpr x
        -- print . show $ getAnnotation <$> inferExpr (constWrap x)
        putStrLn "------------------------------------------------------------"
    -- void runTests


