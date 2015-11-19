import Data.Char
import Data.Set


countCaps :: String -> Int
countCaps [] = 0 
countCaps (x:xs) = if (isUpper x) then (1 + (countCaps xs)) else (countCaps xs)


partition :: (a -> Bool) -> [a] -> ([a],[a])
partition f xs = ([x|x<-xs, f x], [x|x<-xs, not(f x)])


data Multi = Fork Int [Multi] deriving (Eq,Show)


listLeaves :: Multi -> [Int]
listLeaves (Fork n []) = [n]
listLeaves (Fork _ xs) = concatMap listLeaves xs


data BinTree = Empty | Branch Int BinTree BinTree
     deriving (Eq,Show)

weight :: BinTree -> Int
weight (Branch n lB rB) = n + (weight lB) + (weight rB)
weight (Branch n lB _) = n + (weight lB)
weight (Branch n _ rB) = n + (weight rB)
weight Empty = 0

bal :: BinTree -> (Int, Bool)
bal (Branch n Empty Empty) = (n, True)
bal (Branch n lB rB) = ((n+(weight lB)+(weight rB)), (weight lB) == (weight rB))


t1 = (Branch 4 (Branch 3 (Branch 2 Empty Empty) Empty) (Branch 5 Empty Empty))

--autumn :: Multi -> Multi
--autumn (Fork n xs) = (Fork n (if (isLeaf xs) then xs else (autumn xs)))


isLeaf :: Multi -> Bool
isLeaf (Fork _ []) = True
isLeaf (Fork _ _) = False



type PredName = String
type VarName = String
data Exp = Pred PredName [VarName] | Not Exp | And Exp Exp
     | Or Exp Exp | Forall VarName Exp | Exists VarName Exp
     deriving (Show,Eq)



freeVars :: Exp -> [VarName]
freeVars (Pred xs ys) = ys
freeVars (Not exp) = freeVars exp
freeVars (And lexp rexp) = union (freeVars lexp) (freeVars rexp)
freeVars (Or lexp rexp) = union (freeVars lexp) (freeVars rexp)
freeVars (Forall vn exp) = delete vn (freeVars exp)
freeVars (Exists vn exp) = delete vn (freeVars exp)




