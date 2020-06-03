module Lib where

import Regex
import Parser

matchedWords :: [String] -> RegEx -> [String] -> [String]
matchedWords [] exp matched = matched
matchedWords (word : restOfWords) exp matched = if checkStr word exp
                                            then matchedWords restOfWords exp (matched ++ [word])
                                            else matchedWords restOfWords exp matched

match :: String -> String -> IO ()
match str exp=putStrLn (case matchedWords (words str) (parseExp exp) [] of
                            [] -> "No words were matched!"
                            someWords -> "Matched words are: " ++ show someWords)
