{-# LANGUAGE CPP #-}
module Main where

import qualified Fresh.Kind as Kind
import qualified Fresh.Func as Func

#ifdef QC

main = do
    putStrLn "Running tests."
    Kind.runTests
    Func.runTests

#else

main = do
    return ()

#endif
