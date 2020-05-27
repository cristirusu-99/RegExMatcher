module Regex(RegEx, (<+>), (<.>), many, catString, checkStr, emptyExp) where

data RegEx = Phi                --empty language
           | Eps                --empty word
           | Lit Char           --match a character
           | Union RegEx RegEx  --match either of the expressions
           | Cat RegEx RegEx    --catenation of expressions
           | Many RegEx         --expression repeats 0+ times
           deriving (Eq)
           
instance Show RegEx where
    show Phi = "{}"
    show Eps = "()"
    show (Lit l) = [l]
    show (Union exp1 exp2) = "(" ++ show exp1 ++ "+" ++ show exp2 ++ ")"
    show (Cat Eps exp) = show exp
    show (Cat exp Eps) = show exp
    show (Cat exp1 exp2) = show exp1 ++ show exp2
    show (Many (Lit l)) = show lit ++ "*" where lit = Lit l
    show (Many exp) = "(" ++ show exp ++ ")*"

catString :: String -> RegEx
catString "" = Eps
catString (ch:"") = Lit ch
catString (ch:rest) = catString [ch] <.> catString rest

infixl 6 <+>
infixl 7 <.>

emptyExp :: RegEx
emptyExp = Eps

(<+>) :: RegEx -> RegEx -> RegEx
Phi <+> exp = exp
exp <+> Phi = exp
exp1 <+> exp2 = Union exp1 exp2

(<.>) :: RegEx -> RegEx -> RegEx
Phi <.> _ = Phi         --not sure
_ <.> Phi = Phi         --not sure
Eps <.> exp = exp
exp <.> Eps = exp
exp1 <.> exp2 = Cat exp1 exp2

many :: RegEx -> RegEx
many Phi = Eps
many Eps = Eps
many (Many exp) = Many exp
many exp = Many exp

isEmpty :: RegEx -> Bool        --de schimbat sa returneze Eps sau Phi
isEmpty Phi = False
isEmpty Eps = True
isEmpty (Lit _) = False
isEmpty (Union exp1 exp2) = isEmpty exp1 || isEmpty exp2
isEmpty (Cat exp1 exp2) = isEmpty exp1 && isEmpty exp2
isEmpty (Many exp) = True

derivRE :: RegEx -> Char -> RegEx
derivRE Phi _ = Phi
derivRE Eps _ = Phi
derivRE (Lit l) ch
    | l == ch = Eps
    | otherwise = Phi
derivRE (Union exp1 exp2) ch =
    derivRE exp1 ch <+> derivRE exp2 ch
derivRE (Cat exp1 exp2) ch =
    if isEmpty exp1
    then (derivRE exp1 ch <.> exp2) <+> derivRE exp2 ch
    else derivRE exp1 ch <.> exp2
derivRE (Many exp) ch =
    derivRE exp ch <.> many exp

checkMatch :: String -> RegEx -> RegEx
checkMatch "" exp = exp
checkMatch str Phi = Phi
checkMatch str Eps = Phi
checkMatch (ch:rest) exp = checkMatch rest (reduceRedundantUnion (derivRE exp ch))

checkStr :: String -> RegEx -> Bool
checkStr str exp = isEmpty (checkMatch str (reduceRedundantUnion exp))

reduceRedundantUnion :: RegEx -> RegEx
reduceRedundantUnion Phi = Phi
reduceRedundantUnion Eps = Eps
reduceRedundantUnion (Lit l) = Lit l
reduceRedundantUnion (Union exp1 exp2) = 
    if exp1 == exp2
    then reduceRedundantUnion exp1
    else reduceRedundantUnion exp1 <+> reduceRedundantUnion exp2
reduceRedundantUnion (Cat exp1 exp2) = reduceRedundantUnion exp1 <.> reduceRedundantUnion exp2
reduceRedundantUnion (Many exp) = many (reduceRedundantUnion exp)

patt = catString "abc" <+> catString "def"

redTest = (catString "abc" <+> (catString "d" <+> catString "d")) <.> (catString "abc" <+> (catString "d" <+> catString "d"))
        -- ((abc+(d+d))(abc+(d+d)))*

checkTest = checkStr "abcabc" redTest

checkRet = checkMatch "cba" redTest