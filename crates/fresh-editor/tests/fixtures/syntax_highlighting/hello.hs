-- Haskell syntax highlighting test
module Main where

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    let message = greet "World"
    putStrLn message
    print (factorial 10)
