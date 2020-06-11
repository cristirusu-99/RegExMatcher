module Regex(RegEx, (<+>), (<.>), many, catString, checkStr, emptyExp) where

--Tip de date ce implementeaza o expresie regulata
data RegEx = Phi                --Limbajul vid
           | Eps                --Cuvantul vid
           | Lit Char           --Un literal
           | Union RegEx RegEx  --Uniunea a doua expresii RegEx
           | Cat RegEx RegEx    --Catenarea a doua expresii RegEx
           | Many RegEx         --O expresie RegEx repetata de 0 sau mai multe ori - Kleene star
           deriving (Eq)

--Instatiere a functiei show pentru a afisa o expresie RegEx
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

--Functie care returneaza un RegEx asociat catenarii caracterelor dintr-ul String dat
catString :: String -> RegEx
catString "" = Eps
catString (ch:"") = Lit ch
catString (ch:rest) = catString [ch] <.> catString rest

--Setarea precedentei operatorilor <+> si <.>, si a posibilitatii acestora de a fi folositi in forma infiza
infixl 6 <+>
infixl 7 <.>

--Constructor prentru un RegEx ce reprezinta cuvantul vid
emptyExp :: RegEx
emptyExp = Eps

--Opreator binar ce realizeaza uniunea a doua expresii RegEx
(<+>) :: RegEx -> RegEx -> RegEx
Phi <+> exp = exp
exp <+> Phi = exp
exp1 <+> exp2 = Union exp1 exp2

--Opreator binar ce realizeaza catenarea a doua expresie RegEx
(<.>) :: RegEx -> RegEx -> RegEx
Phi <.> _ = Phi         --not sure
_ <.> Phi = Phi         --not sure
Eps <.> exp = exp
exp <.> Eps = exp
exp1 <.> exp2 = Cat exp1 exp2

--Opreator unar ce realizeaza aplicarea unui Kleene star peste o expresie RegEx
many :: RegEx -> RegEx
many Phi = Eps
many Eps = Eps
many (Many exp) = Many exp
many exp = Many exp

--Functie ce verifica daca o expresie RegEx poate fi redusa la cuvantul vid
isEmpty :: RegEx -> Bool        --de schimbat sa returneze Eps sau Phi
isEmpty Phi = False
isEmpty Eps = True
isEmpty (Lit _) = False
isEmpty (Union exp1 exp2) = isEmpty exp1 || isEmpty exp2
isEmpty (Cat exp1 exp2) = isEmpty exp1 && isEmpty exp2
isEmpty (Many exp) = True

--Functie ce realizeaza derivarea Brzozowski a unei expresii RegEx dupa un anumit caracter
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

--Functie ce calculeaza derivarea Brzozowski a unei expresii RegEx pe un cuvant dat
checkMatch :: String -> RegEx -> RegEx
checkMatch "" exp = exp
checkMatch str Phi = Phi
checkMatch str Eps = Phi
checkMatch (ch:rest) exp = checkMatch rest (reduceRedundantUnion (derivRE exp ch))

--Functie ce verifica daca un cuvant dat satisface o expresie RegEx data in functiei de rezultatul derivarii Brzozowski a expresiei
checkStr :: String -> RegEx -> Bool
checkStr str exp = isEmpty (checkMatch str (reduceRedundantUnion exp))

--Functie care elimina instantele de uniune redundanta dintr-o expresie RegEx data
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


-- patt = catString "abc" <+> catString "def"

-- redTest = (catString "abc" <+> (catString "d" <+> catString "d")) <.> (catString "abc" <+> (catString "d" <+> catString "d"))
--         -- ((abc+(d+d))(abc+(d+d)))*

-- checkTest = checkStr "abcabc" redTest

-- checkRet = checkMatch "cba" redTest