{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestParse (runTests) where

import           Test.QuickCheck
import           Data.DeriveTH

import Fresh.Parse.ParseAST
import Fresh.Parse.Parse (parse)
import Fresh.Parse.Lexer (lexer)

import Fresh.Parse.Pretty ()
import Text.PrettyPrint.ANSI.Leijen (Pretty(..))

genStrChoose :: [String] -> Gen String
genStrChoose options = oneof (map pure options)

lowerCase :: Gen Char
lowerCase = oneof (map pure ['a'..'z'])

upperCase :: Gen Char
upperCase = oneof (map pure ['A'..'Z'])

instance Arbitrary a => Arbitrary (Op a) where
    arbitrary = Op <$> arbitrary <*> (genStrChoose ["~","!","#","$","%","^","&","*","-","+","/"])

instance Arbitrary a => Arbitrary (VarName a) where
    arbitrary = VarName <$> arbitrary <*> (lowerCase >>= (\c -> (c:) <$> arbitrary))

instance Arbitrary a => Arbitrary (ConstrName a) where
    arbitrary = ConstrName <$> arbitrary <*> (upperCase >>= (\c -> (c:) <$> arbitrary))

derive makeArbitrary ''FieldName
derive makeArbitrary ''TVarName
derive makeArbitrary ''TypeName

derive makeArbitrary ''FuncArg
derive makeArbitrary ''TypeSpec
derive makeArbitrary ''PatternConstr
derive makeArbitrary ''PatternMatch
derive makeArbitrary ''SwitchCase
derive makeArbitrary ''CallForm
derive makeArbitrary ''Expr
derive makeArbitrary ''TUnion
derive makeArbitrary ''ConstrDef
derive makeArbitrary ''ConstrArg
derive makeArbitrary ''Stmt


prop_unparse :: Stmt () -> Bool
prop_unparse s = s == (fmap (const ()) parsedStmt)
    where [parsedStmt] = parse . lexer . show . pretty $ s

return []

runTests :: Int -> IO Bool
runTests testCount = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = testCount })
