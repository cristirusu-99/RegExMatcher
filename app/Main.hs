module Main where

import Lib

main :: IO ()
main = do
    putStrLn "String to match:"
    str <- getLine
    putStrLn "Regular Expression:"
    exp <- getLine
    match str exp
