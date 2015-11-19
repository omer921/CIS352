-- Omer Winrauke

module AS1 where

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

data Aexp = Num Int | Add Aexp Aexp | Sub Aexp Aexp | Mult Aexp Aexp
          | Div Aexp Aexp | Cond Aexp Aexp Aexp 
            deriving (Eq,Show)
--            deriving (Eq)

------------------------------------------------------------------------------
-- Display

{- 
instance Show Aexp where
    show = showAexp 
-}

showAexp :: Aexp -> String
showAexp (Num n)      = show n
--  showAexp (Var x)      = x   -- not in this version
showAexp (Add e1 e2)  = showBin e1 "+" e2
showAexp (Sub e1 e2)  = showBin e1 "-" e2 
showAexp (Mult e1 e2) = showBin e1 "*" e2 
showAexp (Div e1 e2)  = showBin e1 "/" e2 
showAexp (Cond e0 e1 e2)
         = "("++showAexp e0++"?"++showAexp e1++":"++showAexp e2++")"
-- showAexp (Val n) = show n
showBin :: Aexp -> [Char] -> Aexp -> String
showBin e1 op e2 = "(" ++ showAexp e1 ++ op ++ showAexp e2 ++ ")"
