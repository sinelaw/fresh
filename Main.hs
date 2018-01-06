module Main (main) where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Fresh.Pretty ()
import Fresh.Parse (parse)
import Fresh.Lexer (lexer)
-- import Fresh.Expr  (getAnnotation)
-- import Fresh.Infer (inferExpr)

-- import Text.PrettyPrint.ANSI.Leijen (Pretty(..))

main :: IO ()
main = do
    s <- getContents
    print $ lexer s
    let p = parse . lexer $ s
    print p
