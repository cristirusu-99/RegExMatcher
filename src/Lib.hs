module Lib where

import Regex
import Parser

--Functie care returneaza String-urile, dintr-o lista data, ce satisfac o expresie regulata data
matchedWords :: [String] -> RegEx -> [String] -> [String]
matchedWords [] exp matched = matched
matchedWords (word : restOfWords) exp matched = if checkStr word exp
                                            then matchedWords restOfWords exp (matched ++ [word])
                                            else matchedWords restOfWords exp matched

--Functie care afiseaza daca cuvintele, separate prin ' ', dintr-un String satisfac o expresie regulata data, si care sunt acestea
match :: String -> String -> IO ()
match str exp=putStrLn (case matchedWords (words str) (parseExp exp) [] of
                            [] -> "No words were matched!"
                            someWords -> "Matched words are: " ++ show someWords)
