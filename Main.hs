module Main (main) where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Fresh.Pretty ()
import Fresh.Parse.Parse (parse)
import Fresh.Parse.Lexer (lexer)
import Fresh.Parse.Pretty ()

import Control.Monad (forM_)
-- import Fresh.Expr  (getAnnotation)
-- import Fresh.Infer (inferExpr)

import Text.PrettyPrint.ANSI.Leijen (Pretty(..))

main :: IO ()
main = do
    s <- getContents
    -- print $ lexer s
    let ps = parse . lexer $ s
    -- TODO: ParseAST -> Expr -> inference
    forM_ ps $ \p -> print (pretty p)
