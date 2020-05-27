module Parser(parseExp) where

import Regex


skipTillClosed :: String -> Int -> String
skipTillClosed str 0 = str
skipTillClosed ('(':rest) nOfParan = skipTillClosed rest (nOfParan + 1)
skipTillClosed (')':rest) nOfParan = skipTillClosed rest (nOfParan - 1)
skipTillClosed (ch:rest) nOfParan = skipTillClosed rest nOfParan

getParanCont :: String -> Int -> String -> String
getParanCont str 0 cont = cont
getParanCont (ch:rest) nOfParan cont = case ch of
                                            '(' -> getParanCont rest (nOfParan + 1) (cont ++ [ch])
                                            ')' -> getParanCont rest (nOfParan - 1) (case nOfParan of
                                                                                            1 -> cont
                                                                                            _ -> cont ++ [ch])
                                            _   -> getParanCont rest nOfParan (cont ++ [ch])

parseRules :: String -> RegEx -> RegEx
parseRules "" tempExp = tempExp
parseRules ('(':rest) tempExp = case skipResult of
                                    ('*':finalRest) -> parseRules finalRest tempExp <.> many (parseRules paranCont emptyExp)
                                                        where paranCont = getParanCont rest 1 ""
                                    _ -> parseRules skipResult (tempExp <.> parseRules paranCont emptyExp)
                                        where paranCont = getParanCont rest 1 ""
                                    where skipResult = skipTillClosed rest 1
parseRules ('+':rest) tempExp = tempExp <+> parseRules rest emptyExp
parseRules (ch:rest) tempExp = parseRules rest tempExp <.> catString [ch]

paranLits :: String -> String -> String
paranLits "" str = str
paranLits ('(':ch:')':rest) str = paranLits rest (concat [str, "(" ++ [ch] ++ ")"])
paranLits ('(':rest) str = paranLits rest (concat [str, "("])
paranLits (')':rest) str = paranLits rest (concat [str, ")"])
paranLits ('+':rest) str = paranLits rest (concat [str, "+"])
paranLits ('*':rest) str = paranLits rest (concat [str, "*"])
paranLits (ch:rest) str = paranLits rest (concat [str, "(" ++ [ch] ++ ")"])

parseExp :: String -> RegEx
parseExp str =parseRules (paranLits str "") emptyExp