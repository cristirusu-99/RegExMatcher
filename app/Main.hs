module Main where

import Lib
import Parser
import Regex

main :: IO ()
main = do
    putStrLn "String to match:"
    str <- getLine
    -- putStrLn str
    putStrLn "Regular Expression:"
    exp <- getLine
    -- print (reduceRedundantUnion (parseExp exp))
    match str exp
