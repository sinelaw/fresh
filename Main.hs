{-# LANGUAGE CPP #-}
module Main where

import qualified Fresh.Kind as Kind

#ifdef QC

main = do
    putStrLn "Running tests."
    Kind.runTests

#else

main = do
    return ()

#endif
