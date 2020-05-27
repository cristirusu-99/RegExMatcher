module Lib where

import Regex
import Parser

match :: String -> String -> IO ()
match str exp=putStrLn (if checkStr str (parseExp exp)
                        then "Expression Matched!"
                        else "Expression Not Matched!")
