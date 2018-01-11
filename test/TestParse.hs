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

nameChar :: Gen Char
nameChar = oneof (map pure $ ['a'..'z']++['A'..'Z']++['0'..'9']++['_'])

nameStr :: Gen String
nameStr = listOf nameChar

instance Arbitrary a => Arbitrary (Op a) where
    arbitrary = Op <$> arbitrary <*> (genStrChoose ["~","!","#","$","%","^","&","*","-","+","/"])

instance Arbitrary a => Arbitrary (VarName a) where
    arbitrary = VarName <$> arbitrary <*> (lowerCase >>= (\c -> (c:) <$> nameStr))

instance Arbitrary a => Arbitrary (ConstrName a) where
    arbitrary = ConstrName <$> arbitrary <*> (upperCase >>= (\c -> (c:) <$> nameStr))

instance Arbitrary a => Arbitrary (FieldName a) where
    arbitrary = FieldName <$> arbitrary <*> (lowerCase >>= (\c -> (c:) <$> nameStr))

instance Arbitrary a => Arbitrary (TVarName a) where
    arbitrary = TVarName <$> arbitrary <*> (lowerCase >>= (\c -> (c:) <$> nameStr))

instance Arbitrary a => Arbitrary (TypeName a) where
    arbitrary = TypeName <$> arbitrary <*> (upperCase >>= (\c -> (c:) <$> nameStr))

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
