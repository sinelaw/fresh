{-# LANGUAGE CPP #-}
module Main where

import Fresh.Kind

#ifdef QC

main = do
    putStrLn "Running tests."
    Fresh.Kind.runTests

#else

main = do
    return ()

#endif
