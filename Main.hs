{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- import           Test.QuickCheck
-- import           Data.DeriveTH

import           Control.Monad   (forM_)
import Data.String (IsString(..))
import Fresh.Pretty ()
import Fresh.Kind (Kind(..))
import Fresh.Types (QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..))
import Fresh.Expr  (EVarName(..), Lit(..), Expr(..), getAnnotation)
import Fresh.Infer (inferExpr)

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

-- (~::) :: Expr () -> QualType Type -> Expr ()
-- (~::) = flip $ EAsc ()

(~>) :: EVarName -> Expr () -> Expr ()
(~>) = ELam ()

tcon :: String -> Type
tcon x = Fix $ TyCon $ TCon (Id x) Star

_Bool :: Type
_Bool = tcon "Bool"

_Number :: Type
_Number =  tcon "Number"

(~=>) :: [Pred t] -> t -> QualType t
(~=>) = QualType

wrapFooLet :: Expr () -> Expr ()
wrapFooLet x = let_ "foo" x $ var "foo"

exampleApIdNum = "x" ~> (var "x") ~$ num 2

examples = [ exampleApIdNum
           -- , exampleApIdNum ~:: ([] ~=> _Bool)
--           , exampleApIdNum ~:: ([] ~=> _Number)
           , let_ "x" (num 3) $ var "x"
           , let_ "id" ("x" ~> var "x") $ var "id"
           , wrapFooLet ("y" ~> (let_ "id" ("x" ~> var "y") $ var "id"))
           , wrapFooLet ("y" ~> ("x" ~> var "y"))
           ]

-- ----------------------------------------------------------------------

-- instance Arbitrary (t (Fix t)) => Arbitrary (Fix t) where
--     arbitrary = Fix <$> arbitrary

-- derive makeArbitrary ''GenVar
-- derive makeArbitrary ''Id
-- derive makeArbitrary ''Kind
-- derive makeArbitrary ''TCon
-- derive makeArbitrary ''TypeAST
-- derive makeArbitrary ''Pred
-- derive makeArbitrary ''QualType
-- derive makeArbitrary ''Class

-- derive makeArbitrary ''Lit
-- derive makeArbitrary ''EVarName
-- derive makeArbitrary ''Expr

-- prop_constExpand :: Expr () -> Bool
-- prop_constExpand expr = inferExpr expr == inferExpr (("x" ~> expr) ~$ num 0)

-- return []

-- runTests :: IO Bool
-- runTests = $verboseCheckAll

main :: IO ()
main = do
    forM_ examples $ \x -> do
        print $ pretty x
        print . pretty $ getAnnotation <$> inferExpr x
    -- void runTests


-- {-# LANGUAGE CPP #-}
-- module Main where

-- import qualified Fresh.Kind as Kind

-- #ifdef QC

-- main = do
--     putStrLn "Running tests."
--     Kind.runTests

-- #else

-- main = do
--     return ()

-- #endif
