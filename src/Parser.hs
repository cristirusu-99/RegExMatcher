module Parser(parseExp, paranCat) where

import Regex


skipTillClosed :: String -> Int -> String
skipTillClosed (ch:rest) nOfParan = case ch of
                                        '(' -> skipTillClosed rest (nOfParan + 1)
                                        ')' -> case nOfParan of
                                                    1 -> rest
                                                    _ -> skipTillClosed rest (nOfParan - 1)
                                        _   -> skipTillClosed rest nOfParan

getParanCont :: String -> Int -> String -> String
getParanCont (ch:rest) nOfParan cont = case ch of
                                            '(' -> getParanCont rest (nOfParan + 1) (cont ++ [ch])
                                            ')' -> case nOfParan of
                                                        1 -> cont
                                                        _ -> getParanCont rest (nOfParan - 1) (cont ++ [ch])
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
-- parseRules (ch:rest) tempExp = emptyExp

parseRulesP :: String -> RegEx
parseRulesP "" = emptyExp
parseRulesP ('(':ch:')':rest) = case rest of
                                    ('*':'+':finalRest) -> many (catString [ch]) <+> parseRulesP finalRest
                                    ('*':finalRest) -> many (catString [ch]) <.> parseRulesP finalRest
                                    ('+':finalRest) -> catString [ch] <+> parseRulesP finalRest
                                    _ ->  catString [ch] <.> parseRulesP rest
parseRulesP ('(':rest) = case skipResult of
                                -- ('*':'+':finalRest) -> many (parseRulesP paranCont) <+> parseRulesP finalRest
                                --                         where paranCont = getParanCont rest 1 ""
                                -- ('*':finalRest) -> many (parseRulesP paranCont) <.> parseRulesP finalRest
                                --                     where paranCont = getParanCont rest 1 ""
                                "" -> parseRulesP paranCont
                                        where paranCont = getParanCont rest 1 ""
                                ('(':ch:')':finalRest) -> case finalRest of
                                                        ('*':'+':finallyRest) -> many (catString [ch]) <+> parseRulesP finallyRest
                                                        ('*':finallyRest) -> many (catString [ch]) <.> parseRulesP finallyRest
                                                        ('+':finallyRest) -> catString [ch] <+> parseRulesP finallyRest
                                                        _ ->  catString [ch] <.> parseRulesP finalRest
                                ('(':finalRest) -> case paranCont of
                                            (ch:"") -> catString [ch] <.> parseRulesP skipResult
                                            _ -> parseRulesP paranCont <.> parseRulesP skipResult
                                    where paranCont = getParanCont rest 1 ""
                                ('*':finalRest) -> case finalRest of
                                                        "" -> many (parseRulesP paranCont)
                                                                where paranCont = getParanCont rest 1 ""
                                                        ('+':finallyRest) -> many (parseRulesP paranCont) <+> parseRulesP finallyRest
                                                                            where paranCont = getParanCont rest 1 ""
                                                        ('(':_) -> many (parseRulesP paranCont) <.> parseRulesP finalRest
                                                                    where paranCont = getParanCont rest 1 ""
                                                        _ -> emptyExp
                                                        -- _ -> many (parseRulesP paranCont)
                                                        --     where paranCont = getParanCont rest 1 ""
                                ('+':finalRest) -> parseRulesP paranCont <+> parseRulesP finalRest
                                                    where paranCont = getParanCont rest 1 ""
                                -- _ -> case paranCont of
                                --             (ch:"") -> catString [ch] <.> parseRulesP skipResult
                                --             _ -> parseRulesP paranCont <.> parseRulesP skipResult
                                --     where paranCont = getParanCont rest 1 ""
                                _ -> emptyExp
                                where skipResult = skipTillClosed rest 1
-- parseRulesP (ch:rest) = catString [ch] <.> parseRulesP rest

paranLits :: String -> String -> String
paranLits "" str = str
paranLits ('(':ch:')':rest) str = paranLits rest (concat [str, "(" ++ [ch] ++ ")"])
paranLits ('(':rest) str = paranLits rest (concat [str, "("])
paranLits (')':rest) str = paranLits rest (concat [str, ")"])
paranLits ('+':rest) str = paranLits rest (concat [str, "+"])
paranLits ('*':rest) str = paranLits rest (concat [str, "*"])
paranLits (ch:rest) str = paranLits rest (concat [str, "(" ++ [ch] ++ ")"])

paranCat :: String -> String -> String -> String
paranCat "" word str = str ++ word
paranCat (ch:"") word str = str ++ "(" ++ word ++ [ch] ++ ")"
paranCat ('(':rest) word str = case skipResult of
                                    ('*':finalRest) -> paranCat finalRest (concat [word, "(" ++ paranCont ++ ")*"]) str
                                                        where paranCont = paranCat (getParanCont rest 1 "") "" ""
                                    _ -> paranCat skipResult (concat [word, "(" ++ paranCont ++ ")"]) str
                                        where paranCont = paranCat (getParanCont rest 1 "") "" ""
                                    where skipResult = skipTillClosed rest 1
paranCat (ch:'+':rest) word str = paranCat rest "" (concat [str, "(" ++ word ++ [ch] ++ ")" ++ "+"])
paranCat (ch1:ch2:rest) word str = paranCat (ch2:rest) (concat [word, [ch1]]) str

parseExp :: String -> RegEx
-- parseExp str = parseRules (paranLits str "") emptyExp
-- parseExp str = parseRules (paranLits (paranCat str "" "") "") emptyExp
parseExp str = parseRulesP (paranLits (paranCat str "" "") "")
-- parseExp str = parseRulesP (paranLits str "")



-- "(ab)(((dd)*c)+c"

-- ((abc+(d+d))(abc+(d+d)))*
-- (((abc)+((d)+d))((abc)+((d)+d)))*