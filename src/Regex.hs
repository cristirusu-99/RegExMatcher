module Regex(RegEx, (<+>), (<.>), many, catString, derivRE, patt) where

data RegEx = Phi
           | Eps
           | Lit Char
           | Union RegEx RegEx
           | Cat RegEx RegEx
           | Many RegEx
           
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
catString (lit:rest) = Cat (Lit lit) (catString rest)

infixl 6 <+>
infixl 7 <.>

(<+>) :: RegEx -> RegEx -> RegEx
Phi <+> exp = exp
exp <+> Phi = exp
exp1 <+> exp2 = Union exp1 exp2

(<.>) :: RegEx -> RegEx -> RegEx
Phi <.> _ = Phi
_ <.> Phi = Phi
Eps <.> exp = exp
exp <.> Eps = exp
exp1 <.> exp2 = Cat exp1 exp2

many :: RegEx -> RegEx
many Phi = Eps
many Eps = Eps
many (Many exp) = Many exp
many exp = Many exp

isEmpty :: RegEx -> Bool
isEmpty Phi = False
isEmpty Eps = True
isEmpty (Lit _) = False
isEmpty (Union r1 r2) = isEmpty r1 || isEmpty r2
isEmpty (Cat r1 r2) = isEmpty r1 && isEmpty r2
isEmpty (Many r) = True

derivRE :: RegEx -> Char -> RegEx
derivRE Phi _ = Phi
derivRE Eps _ = Phi
derivRE (Lit l1) l2
    | l1 == l2 = Eps
    | otherwise = Phi
derivRE (Union r1 r2) l =
    Union (derivRE r1 l) (derivRE r2 l)
derivRE (Cat r1 r2) l =
    if isEmpty r1
    then Union (Cat (derivRE r1 l) r2) (derivRE r2 l)
    else Cat (derivRE r1 l) r2
derivRE (Many r) l =
    Cat (derivRE r l) (Many r)

patt = catString "abc" <+> catString "def"