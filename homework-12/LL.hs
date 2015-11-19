-- LL: Version 1
module LL where 

import Test.QuickCheck
import Control.Monad
import Data.List

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Data structures
------------------------------------------------------------------------
------------------------------------------------------------------------

data Individual = Var Int 
                | Const String     
                  deriving (Show,Eq)
--                  deriving (Eq)

data Expr = Pred String [Individual] 
          | Forall Int Expr 
          | Exists Int Expr 
          | Not Expr 
          | And Expr Expr 
          | Or Expr Expr
            deriving (Show,Eq)
--            deriving (Eq)

{-
instance Show Individual where show = showIndiv
instance Show Expr       where show = showExpr
-}

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Display
------------------------------------------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
-- parenify b str 
--   if b is true, return str enclosed in parens, o/w  just return str.
parenify b str = if b then enclose str else str

------------------------------------------------------------------------
-- enclose s = the string s enclosed in parens
enclose s = "("++s++")"

------------------------------------------------------------------------
-- showIndiv indv = a string version of indv
showIndiv :: Individual -> String
showIndiv (Var n)   = 'x':show n
showIndiv (Const s) = s

------------------------------------------------------------------------
-- Fully parenthesized printing of expressions
showExpr :: Expr -> String
showExpr (Pred rel is) = rel ++ "[" ++ 
                         (intercalate "," (map showIndiv is)) ++ "]"
showExpr (Forall n e) = enclose("Forall x"++show n++" ("++showExpr e++")")
showExpr (Exists n e) = enclose("Exists x"++show n++" ("++showExpr e++")")
showExpr (Not e)      = '~':enclose (showExpr e)
showExpr (And e1 e2)  = enclose $ showExpr e1++" & "++showExpr e2
showExpr (Or e1 e2)   = enclose $ showExpr e1++" + "++showExpr e2
------------------------------------------------------------------------
-- Left associative printing of expressions

-- construct precedences 
(orPrec,andPrec,allPrec,existsPrec,notPrec,predPrec) = (1,2,3,3,4,5)
prec (Or _ _)     = orPrec
prec (And _ _)    = andPrec
prec (Forall _ _) = allPrec
prec (Exists _ _) = existsPrec
prec (Not _)      = notPrec
prec (Pred _ _)   = predPrec


showExprl :: Expr -> String
showExprl (Pred rel is) 
    = rel ++ "[" ++ (intercalate "," (map showIndiv is)) ++ "]"
showExprl (Forall n e) 
    = "Forall x" ++ show n ++ " ("++showExprl e ++ ")"
showExprl (Exists n e) 
    = "Exists x" ++ show n ++ " (" ++ showExprl e ++ ")"
showExprl (Not e)      
    = '~':parenify ((prec e) <= notPrec) (showExprl e)
showExprl (And e1 e2)  
    = (parenify ((prec e1) < andPrec) (showExprl e1)) 
      ++ " & " ++
      (parenify ((prec e2) <= andPrec) (showExprl e2))
showExprl (Or e1 e2)   
    = (parenify ((prec e1) < orPrec) (showExprl e1))
      ++ " + " ++
      (parenify ((prec e2) <= orPrec) (showExprl e2))


------------------------------------------------------------------------
------------------------------------------------------------------------
-- QuickCheck test generators
------------------------------------------------------------------------
------------------------------------------------------------------------

xcast = ["cheeta","curiousgeorge","grodd",
         "mightyjoe","thelibrarian","wukong"]
xrels = ["Likes","Resents","Suspects","Teases","Tolerates"]

instance Arbitrary Individual where
    arbitrary = oneof [ elements [ Var n   | n <- [0..7]]
                      , elements [ Const c | c <- xcast  ]
                      ]

genPred = do { name <- elements xrels
             ; i1   <- arbitrary
             ; i2   <- arbitrary
             ; return $ Pred name [i1,i2]
             }

instance Arbitrary Expr where
    shrink (Pred _ _) = []
    shrink (Forall _ e) = [e]
    shrink (Exists _ e) = [e]
    shrink (Not e)      = [e]
    shrink (And e1 e2)  = [e1,e2]
    shrink (Or e1 e2)   = [e1,e2]
    arbitrary = sized exp
        where
          exp 0 = genPred
          exp n = frequency [ (1,genPred)
                            , (1, return Not `ap` subexpr)
                            , (1, return And `ap` subexpr `ap` subexpr)
                            , (1, return Or  `ap` subexpr `ap` subexpr)
                            , (2, do { q <- elements [Forall,Exists]
                                     ; n <- elements [0..7]
                                     ; e <- subexpr
                                     ; return $ q n e
                                     })
                            ]
              where subexpr = exp (n `div` 2)
