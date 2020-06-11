module Main where

import Lib
import Parser
import Regex

intro :: IO ()
intro = do
    putStrLn "Hello!"
    putStrLn "This application is designed to show you the words from a string that can be matched with a regular expession of your choice!"
    putStrLn "I hope you will enjoy using this application!"

application :: IO ()
application = do
    putStrLn "String to match:"
    str <- getLine
    putStrLn "Regular Expression:"
    exp <- getLine
    match str exp
    putStrLn "Continue?(Y/N)"
    cont <- getLine
    if cont == "N"
        then putStrLn "Good-bye!"
        else application


main :: IO ()
main = do
    intro
    application
    putStrLn "Press Enter to close this window!"
    closeCommand <- getLine
    putStrLn closeCommand
