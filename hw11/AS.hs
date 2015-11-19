module AS where
import Test.QuickCheck
import Control.Monad(ap)

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

data Exp = Num Int | Add Exp Exp | Sub Exp Exp | Mult Exp Exp | Div Exp Exp
           deriving (Eq,Show)
--           deriving (Eq)

-- instance Show Exp where show e = showExpr e

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Evaluation
eval (Num n) = n
eval (Add  e1 e2) = eval e1 + eval e2
eval (Sub  e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Div  e1 e2) = eval e1 `div` eval e2
-- division by 0 causes an error, as it should

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Display

------------------------------------------------------------------------
-- parens b str 
--   if b is true, return str enclosed in parens, o/w  just return str.
parens b str = if b then "("++str++")" else str

------------------------------------------------------------------------
-- showExpr e returns a fully parenthesized string version of e.
-- E.g., showExpr (Add (Add (Num 1) (Num 2)) (Num 3))
--       returns "((1+2)+3)"
--       showExpr (Add (Num 1) (Add (Num 2) (Num 3)))
--       returns "(1+(2+3))"
showExpr (Num n)      = show n
showExpr (Add e1 e2)  = showBin e1 "+" e2 
showExpr (Sub e1 e2)  = showBin e1 "-" e2 
showExpr (Mult e1 e2) = showBin e1 "*" e2 
showExpr (Div  e1 e2) = showBin e1 "/" e2 

showBin e1 op e2 = parens True $ showExpr e1 ++ op ++ showExpr e2 

------------------------------------------------------------------------
-- prec expr = the procedence number of the top operator of expr
prec (Add _ _)  = 1
prec (Sub _ _)  = 1
prec (Mult _ _) = 2
prec (Div _ _)  = 2
prec (Num _)    = 3

------------------------------------------------------------------------
-- showExprl e = a string version of e where we omit as many parens
--    as possible assuming +,-,*,/ are left-associative.
-- E.g. 
--   showExprl (Add (Add (Num 1) (Num 2)) (Num 3))
--   returns "1+2+3"
--   showExprl (Add (Num 1) (Add (Num 2) (Num 3)))
--   returns "1+(2+3)"

showExprl (Num n)      
    = show n
showExprl (Add e1 e2)  
    = parens (prec e1 < 1) (showExprl e1)
      ++ "+" 
      ++ parens (prec e2 <= 1) (showExprl e2)
showExprl (Sub e1 e2)  
    = parens (prec e1 < 1) (showExprl e1)
      ++ "-" 
      ++ parens (prec e2 <= 1) (showExprl e2)
showExprl (Mult e1 e2) 
    = parens (prec e1 < 2) (showExprl e1)
      ++ "*" 
      ++ parens (prec e2 <= 2) (showExprl e2)
showExprl (Div  e1 e2)  
    = parens (prec e1 < 2) (showExprl e1)
      ++ "/" 
      ++ parens (prec e2 <= 2) (showExprl e2)

------------------------------------------------------------------------
-- showExprr e = a string version of e where we omit as many parens
--    as possible assuming +,-,*,/ are right-associative.
-- E.g. 
--   showExprr (Add (Add (Num 1) (Num 2)) (Num 3))
--   returns "(1+2)+3"
--   showExprr (Add (Num 1) (Add (Num 2) (Num 3)))
--   returns "1+2+3"
showExprr (Num n)      = show n
showExprr (Add e1 e2)  
    = parens (prec e1 <= 1) (showExprr e1)
      ++ "+" 
      ++ parens (prec e2 < 1) (showExprr e2)
showExprr (Sub e1 e2)  
    = parens (prec e1 <= 1) (showExprr e1)
      ++ "-" 
      ++ parens (prec e2 < 1) (showExprr e2)
showExprr (Mult e1 e2) 
    = parens (prec e1 <= 2) (showExprr e1)
      ++ "*" 
      ++ parens (prec e2 < 2) (showExprr e2)
showExprr (Div  e1 e2)  
    = parens (prec e1 <= 2) (showExprr e1)
      ++ "/" 
      ++ parens (prec e2 < 2) (showExprr e2)

------------------------------------------------------------------------
-- builds a random Expr

instance Arbitrary Exp where
    shrink (Num _)      = []
    shrink (Add e1  e2) = [e1,e2]
    shrink (Sub e1  e2) = [e1,e2]
    shrink (Mult e1 e2) = [e1,e2]
    shrink (Div e1  e2) = [e1,e2]
    arbitrary = sized expr
        where
          genNum = return Num `ap` (elements [0..20])
          expr n 
              | n <= 0    = genNum
              | otherwise = oneof [ genNum
                                  , return Add  `ap` subform `ap` subform 
                                  , return Sub  `ap` subform `ap` subform 
                                  , return Mult `ap` subform `ap` subform 
                                  , return Div  `ap` subform `ap` subform 
                                  ]
                            where
                              subform = expr (n `div` 2)
