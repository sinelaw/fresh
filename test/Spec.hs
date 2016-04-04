{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Test.QuickCheck


import           Control.Monad   (void)
import Data.String (IsString(..))
import Fresh.Pretty ()
import Fresh.Kind (Kind(..))
import Fresh.Type (inferExpr, EVarName(..), Lit(..), Expr(..), QualType(..), Type, Fix(..), TypeAST(..), TCon(..), Id(..), Pred(..))

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

exampleNumber :: Expr (Maybe (QualType Type))
exampleNumber = inferExpr exampleApIdNum

exampleBadAsc :: Expr (Maybe (QualType Type))
exampleBadAsc = inferExpr $ exampleApIdNum ~:: ([] ~=> _Bool)

exampleAsc :: Expr (Maybe (QualType Type))
exampleAsc = inferExpr $ exampleApIdNum ~:: ([] ~=> _Number)

exampleLet :: Expr (Maybe (QualType Type))
exampleLet = inferExpr $ let_ "id" ("x" ~> var "x") $ var "id"

exampleLet2 :: Expr (Maybe (QualType Type))
exampleLet2 = inferExpr $ wrapFooLet ("y" ~> (let_ "id" ("x" ~> var "y") $ var "id"))

exampleLam2 :: Expr (Maybe (QualType Type))
exampleLam2 = inferExpr $ wrapFooLet ("y" ~> ("x" ~> var "y"))

-- ----------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $verboseCheckAll

main :: IO ()
main = void runTests


