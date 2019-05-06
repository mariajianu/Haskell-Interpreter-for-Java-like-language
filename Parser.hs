module Parser
where

import Util
import ClassState
import Data.Maybe
import Data.List (sort)
import InferenceDataType

import qualified Data.List as List
import qualified Data.Char as Char

-- Definire Program
data Program = Program [ClassState]

initEmptyProgram :: Program
initEmptyProgram = Program ([ClassState "Global" "Global" (ContainerVar []) (ContainerFunct [])])


-- toate variabilele sunt in clasa Global
getVars :: Program -> [[String]]
getVars (Program []) = []
getVars (Program ((ClassState _ _ (ContainerVar var) _ ):l)) = List.nub (var ++ (getVars (Program l)))

getName :: ClassState -> String
getName (ClassState name _ _ _) = name

setNameNoParent :: String -> ClassState
setNameNoParent name = (ClassState name "Global" (ContainerVar []) (ContainerFunct []))

-- primeste nume clasa nume parinte
setNameWithParent :: String -> String -> ClassState
setNameWithParent name pname = (ClassState name pname (ContainerVar []) (ContainerFunct []))

getClasses :: Program -> [String]
getClasses (Program []) = []
getClasses (Program (x:l)) = if getName x == "null" then getClasses (Program (List.nub l)) 
                             else List.nub (sort ((getName x) : getClasses (Program l)))

getParent :: ClassState -> String
getParent (ClassState _ pname _ _) = pname

-- numele clasei caruita ii cautam parintele si lista de clase
getParentClass :: String -> Program -> String
getParentClass _ (Program []) = []
getParentClass name (Program (x:l)) = if name == getName x then getParent x 
                                                           else getParentClass name (Program l)

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass _ (Program []) = []
getFuncsForClass name (Program (x:l)) = if name == getName x then getValues x Func 
                                                             else getFuncsForClass name (Program l)

-- Instruction va fi o lista de String-uri care vor reprezenta String - uri de tipul:
-- class ClassName
-- newvar VarName
-- Double f::a(double,int)
-- adica fiecare linie din fisier
data Instruction = Instruction String deriving Show

-- al doilea String e acumulatorul de String si al treilea e acumulatorul de lista
getParts :: String -> String -> [String] -> [String]
getParts [] [] list = list
getParts [] acc list = acc : list
getParts (x:l) acc list = if Char.isControl x then getParts l [] (list ++ acc:[]) else getParts l (acc++x:[]) list

--getParts ne intoarce o lista cu fiecare linie din fisier:
--ex: ["class Double", "newvar a = Double"]
getInstructionList :: [String] -> [Instruction] -> [Instruction]
getInstructionList [] acc = acc
getInstructionList (x:l) acc = getInstructionList l acc ++ (Instruction x):[]

--lista de instructiuni
parse :: String -> [Instruction]
parse input = reverse (getInstructionList (getParts input [] []) [])

-- s = class Name extends Name2
-- intoarce numele clasei adica Nume
getClassName :: String -> String
getClassName s = (head (drop 1 (words s)))

charFound :: Char -> String -> Bool
charFound _ "" = False
charFound c (x:xs) = if x == c then True else charFound c xs

-- primeste varname=Class
getVarName :: String -> String -> String
getVarName (x:l) acc = if x == '=' || x == ' ' then acc else getVarName l (acc ++ x:[]) 

-- al doilea String e acumulatorul de String si al treilea e acumulatorul de lista
getParams :: String -> String -> [String] -> [String]
getParams [] [] s = s
getParams [] acc list = acc : list
getParams (x:xs) acc list = if Char.isLetter x then getParams xs (acc++x:[]) list else getParams xs [] (list ++ acc:[])

clearList :: [String] -> [String]
clearList l = filter (\x -> x /= "") l

-- primeste o lista de stringuri si verifica daca fiecare se afla in clasele din Program 
checkParams :: Eq a => [a] -> [a] -> Bool
checkParams s1 s2 = all (\x -> elem x s2) s1

--intoarce clasa de care apartine functia
getMainClass :: String -> String -> String
getMainClass [] acc = acc
getMainClass (x:xs) acc = if x /= ':' && x /= ' ' then getMainClass xs (acc++x:[]) else acc

-- converteste lista de String-uri intr-un singur cuvant pentru a fi mai usor de parsat
fromListToString :: [String] -> String
fromListToString [] = []
fromListToString (x:xs) = x ++ fromListToString xs

-- parseaza inputul pentru a determina numele functiei
getFunctionName :: String -> String -> String
getFunctionName [] acc = acc
getFunctionName (x:xs) acc 
    | x == '(' = acc
    | x == ' ' = getFunctionName xs acc
    | otherwise = getFunctionName xs (acc ++ x:[])

functionNameHelper :: String -> String
functionNameHelper [] = []
functionNameHelper (x:xs) = if x == ':' then (drop 1 xs) else functionNameHelper xs

-- primeste stringul de la (param1 ..)
paramsHelper :: String -> String
paramsHelper (x:xs) = if x == '(' then ('(' : xs) else paramsHelper xs

-- constructFunction e apelata cu toata linia
constructFunction :: String -> [String]
constructFunction s  = (functionNameHelper (getFunctionName s []) : head(words s): []) 
                        ++ (clearList (getParams (paramsHelper s) [] []))

-- intoarce o clasa cu numele dat din program
findClassState :: String -> Program -> ClassState
findClassState name (Program []) = initEmptyClass
findClassState name (Program (x:xs)) = if name == getName x then x else findClassState name (Program xs)

-- adaugam o functie in lista de functii ale unei clase
updateClassState :: String -> ClassState -> ClassState
updateClassState s (ClassState nameC nameP (ContainerVar var) (ContainerFunct funct)) 
                    = (ClassState nameC nameP (ContainerVar var) (ContainerFunct (constructFunction s : funct)))

-- adauga o functie in program
addFunctInProgram :: String -> Program -> ClassState
addFunctInProgram [] (Program []) = initEmptyClass
addFunctInProgram s (Program p) = updateClassState s (findClassState (getMainClass (fromListToString (drop 1 (words s))) []) (Program p))
     
-- nume var, nume clasa
addVarInProgram :: String -> String -> Program -> ClassState
addVarInProgram var classN p = (ClassState classN (getParentClass classN p) (ContainerVar ([var, classN]:[])) (ContainerFunct []))

interpret :: Instruction -> Program -> Program
interpret (Instruction []) (Program p) = (Program p)
interpret (Instruction s) (Program p)
    -- suntem in cazul class NumeClasa extends AltaClasa
    | elem "extends" (words s) == True = if elem (head (drop 1 (words s))) (getClasses (Program p)) then (Program p)
                                         else if elem (head (reverse (words s))) (getClasses (Program p)) 
                                              then Program ((setNameWithParent (getClassName s) (head (reverse (words s))) : p))
                                              else Program ((setNameNoParent (getClassName s)) : p)
    --suntem in cazul class NumeClasa
    | head(words s) == "class" = if elem (head (reverse (words s))) (getClasses (Program p)) 
                                 then (Program p)
                                 else Program ((setNameNoParent (head (reverse (words s))) : p))
   
    -- suntem in cazul newvar a egal NumeCLasa
    | head(words s) == "newvar" = if elem (reverse (getVarName (reverse s) [])) (getClasses (Program p))
                                  then Program ((addVarInProgram (getVarName (fromListToString (drop 1 (words s))) []) 
                                                  (reverse (getVarName (reverse s) [])) (Program p)) : p) 
                                  else (Program p)
    -- suntem in cazul declaratie de functie
    -- primele 2 verificari sunt pentru numele clasei careia aprtine si tipul returnat
    -- la a treia verificam daca declaratia de functie e valida
    | charFound ':' s == True = if elem (head (words s)) (getClasses (Program p)) || elem (getClassName s) (getClasses (Program p))
                                then if (checkParams (clearList (getParams (paramsHelper s) [] [])) (getClasses (Program p))) == False 
                                     then (Program p)
                                     else Program(addFunctInProgram s (Program p) : p)
                                else (Program p)
    | elem "infer" (words s) = (Program p)

-- primeste numele variabilei, o cauta in var din Global si ii intoarce tipul daca ea exista
findVar :: String -> ClassState -> Maybe String
findVar name (ClassState cname pname (ContainerVar []) func) = Nothing 
findVar name (ClassState cname pname (ContainerVar (x:l)) func) = if name == head x then Just (head (drop 1 x)) 
                                                                  else findVar name (ClassState cname pname (ContainerVar l) func)

-- intoarce clasa Global
getGlobal :: Program -> ClassState
getGlobal (Program (x:l)) = if getName x == "Global" then x else getGlobal (Program l) 

toString :: Maybe String -> String
toString (Just s) = s
toString Nothing = []

isVar :: Expr -> Bool
isVar (Va a) = True
isVar (FCall s f l) = False

-- verificam daca lista de [Expr] e formaata doar in variabile
justVars :: [Expr] -> Bool
justVars [] = True
justVars (x:l) = if isVar x == True then justVars l else False

-- verificam daca o variabila exista
varExists :: String -> ClassState -> Bool
varExists s cs = if findVar s cs == Nothing then False else True


-- primeste numele variabilei si lista de clase si intoarce clasa de care apartine var
typeVar :: String -> Program -> Maybe ClassState
typeVar  name (Program []) = Nothing
typeVar name (Program (x:l)) = if findVar name x == Nothing then typeVar name (Program l) else Just x

-- primeste clasa ex: Double,String si numele functiei si verifica daca functia exista in clasa
-- trebuie modificat sa caut si in parintii lui classState
functExistsInClass :: Maybe ClassState -> String -> Bool
functExistsInClass Nothing _ = False
functExistsInClass (Just (ClassState name pname (ContainerVar var) (ContainerFunct []))) _ = False
functExistsInClassc (Just (ClassState name pname (ContainerVar var) (ContainerFunct (x:l)))) namef =
     if namef == head x then True 
     else functExistsInClass (Just (ClassState name pname (ContainerVar var) (ContainerFunct l))) namef

getAllVarNames :: [[a]] -> [a] -> [a]
getAllVarNames [] acc = acc
getAllVarNames (x:l) acc = getAllVarNames l (acc ++ (head x):[])

functsInProgram :: [String] -> Program -> Bool
functsInProgram s (Program []) = False
functsInProgram [] _ = True
functsInProgram (x:l) p = if elem x (getAllVarNames (getVars p) []) then functsInProgram l p else False

-- primeste numele functiei si lista de param si intoarce tipul intors daca functia exista sau Nothing
inferSimpleFunct :: String -> [String] -> Program -> Program -> Maybe String
inferSimpleFunct name params (Program []) (Program p) = Nothing
inferSimpleFunct name params (Program ((ClassState _ _ _ (ContainerFunct [])) : ls))  (Program p) 
                             = inferSimpleFunct name params (Program ls) (Program p)
inferSimpleFunct name params (Program ((ClassState cname pname vars (ContainerFunct (x:l))) : ls))  (Program p) 
                              = if name == head x && functsInProgram params (Program p) 
                              then Just (head (drop 1 x))
                              else inferSimpleFunct name params 
                              (Program ((ClassState cname pname vars (ContainerFunct l)) : ls)) (Program p)

getSymbol :: Expr -> String
getSymbol (Va a) = a

getVarList :: [Expr] -> [String] -> [String]
getVarList [] acc = acc
getVarList (x:l) acc = getVarList l (acc ++ ((getSymbol x):[]))

infer :: Expr -> Program -> Maybe String
infer (Va s) (Program p) = findVar s (getGlobal (Program p))
infer (FCall var funct [] ) (Program p) = Nothing
infer (FCall var funct (x:l)) (Program p)
    -- expr de tipul (FCall var funct [Va v1, Vav2 ..])
    | justVars (x:l) = inferSimpleFunct funct (getVarList (x:l) []) (Program p) (Program p)
    | otherwise = infer (FCall var funct l) (Program p)
                              