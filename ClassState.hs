module ClassState
where

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

data ContainerVar = ContainerVar [[String]] deriving Show
data ContainerFunct = ContainerFunct [[String]] deriving Show

-- o singura clasa -> are nume, nume clasa parinte, lista de variabile si lista de functii
data ClassState = ClassState String String ContainerVar ContainerFunct deriving Show

-- pentru a evita ca la fiecare update sa sparg lista de ClassState-uri, sa scot un element,
-- sa pun altul si apoi sa le lipesc, cand adaug un element in pun in lista cu noua modificare 
-- si apoi scot elementul mai vechi cu acelasi nume
instance Eq ClassState where
    ClassState name _ _ _ == ClassState name2 _ _ _ = name == name2


initEmptyClass :: ClassState
initEmptyClass = ClassState "null" "null" (ContainerVar []) (ContainerFunct [])

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState name p (ContainerVar ls) func) Var s = ClassState name p (ContainerVar (ls ++ [s])) func
insertIntoClass (ClassState name p var (ContainerFunct func)) Func s = ClassState name p var (ContainerFunct (func ++ [s]))

getValues :: ClassState -> InstrType -> [[String]]
getValues (ClassState name p (ContainerVar []) (ContainerFunct [])) Var = []
getValues (ClassState name p (ContainerVar var) (ContainerFunct [])) Var = var  
getValues (ClassState name p (ContainerVar ls) func) Var = ls
getValues (ClassState name p _ (ContainerFunct ls)) Func = ls
gatValues (ClassState name p var (ContainerFunct ls)) Func = ls