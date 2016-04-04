{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Test.QuickCheck

import           Data.DeriveTH

import           Control.Monad   (void, forM_)
import Data.String (IsString(..))
import Fresh.Pretty ()
import Fresh.Kind (Kind(..))
import Fresh.Type (inferExpr, EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..), GenVar(..), Class(..), TypeError(..), getAnnotation)
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

tcon :: String -> Fix TypeAST
tcon x = Fix $ TyCon $ TCon (Id x) Star

_Bool :: Fix TypeAST
_Bool = tcon "Bool"

_Number :: Fix TypeAST
_Number =  tcon "Number"

(~=>) :: [Pred t] -> t -> QualType t
(~=>) = QualType

wrapFooLet :: Expr () -> Expr ()
wrapFooLet x = let_ "foo" x $ var "foo"

exampleApIdNum = "x" ~> (var "x") ~$ num 2

examples = [ exampleApIdNum
           -- , exampleApIdNum ~:: ([] ~=> _Bool)
           , exampleApIdNum ~:: ([] ~=> _Number)
           , ELit () (LitBool False)
           , let_ "id" ("x" ~> var "x") $ var "id"
           , wrapFooLet ("y" ~> (let_ "id" ("x" ~> var "y") $ var "id"))
           , wrapFooLet ("y" ~> ("x" ~> var "y"))
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
    forM_ examples $ \x -> do
        print $ pretty x
        print . pretty $ getAnnotation <$> inferExpr x
        -- print . show $ getAnnotation <$> inferExpr x
        -- print . show $ getAnnotation <$> inferExpr (constWrap x)
        putStrLn "------------------------------------------------------------"
    void runTests


