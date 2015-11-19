-- Omer Winrauke

import Test.QuickCheck
import Parsing 
import AS
import DrawExpr
import Data.Char

------------------------------------------------------------------------
pleaseFix = error "Please fix me."
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Hutton Problem 2
comment :: Parser ()
comment = do {
       ; string "//"
       ; (many $ sat(/='\n'))
       ; char '\n'
       ; return()
       }


isHexa :: Char -> Bool
isHexa c = elem c "0123456789ABCDEF"

hexa :: Parser String
hexa = do { char '0';
            char 'x';
            ns <- many $ sat(isHexa);
            return ns
            }

------------------------------------------------------------------------
-- Sample parser 1 for the grammar
--     S ::= X# | #
--     X ::= aXb | eps 
-- where eps stands for the empty string.
-- Note: L(S) = { a^n b^n # | n>=0 }

s1, x1 :: Parser ()
s1 = do { x1
        ; char '#'
        ; return ()
        }
     +++
     do { char '#'
        ; return ()
        }

x1 = do { char 'a'
        ; x1
        ; char 'b'
        ; return ()
        }
       +++
       return ()

ok1  = ["#","ab#","aabb#","aaabbb#","aaaabbbb#","aaaaabbbbb#"]
bad1 = ["a#","abb#","aab#","ba#","aabbb#"]

test1 = all (not.null) (map (parse s1) ok1)
        && 
        all null (map (parse s1) bad1)

------------------------------------------------------------------------
-- Sample parser 2 for the grammar
--     S ::= X# | #
--     X ::= aaXb | eps 
-- where eps stands for the empty string.
-- Note: L(S) = { a^(2n) b^n # | n>=0 }

s2, x2 :: Parser ()
s2 = do { x2            -- S2 ::=  X2 # | #
        ; char '#'
        ; return ()
        }
     +++
     do { char '#'
        ; return ()
        }

x2 = do { char 'a'      -- X2 ::= a a X2 b | eps
        ; char 'a'
        ; x2
        ; char 'b'
        ; return ()
        }
       +++
       return ()

ok2  = ["#","aab#","aaaabb#","aaaaaabbb#","aaaaaaaabbbb#"]
bad2 = ["a#","abb#","abab#","ba#","aaaabbb#"]

test2 = all (not.null) (map (parse s2) ok2)
        && 
        all null (map (parse s2) bad2)

------------------------------------------------------------------------
-- parser for the grammar
--   S ::=  (P) S  |  #
--   P ::=  (P) P  |  eps

s3, p3 :: Parser ()
s3 = do { char '('
        ; p3
        ; char ')'
        ; s3
        }
     +++
     do { char '#'; return () }

p3 = do { char '('
        ; p3
        ; char ')'
        ; p3
        }
     +++
     do { return () }


ok3  = ["#","()#","()()#","(()())#","()(())(()())#","((()())())#"]
bad3 = ["(#","())#",")(#","()(#","((())))#"]

test3 = all (not.null) (map (parse s3) ok3)
        && 
        all null (map (parse s3) bad3)



------------------------------------------------------------------------
-- Hutton Problem 6

-- expr is a parser for 
--    E ::= T + E | T
--    T ::= F * T | F
--    F ::= Num | (E)
-- it evaluates the expression while parsing it.
expr, term, factor :: Parser Int
expr = do { t <- term
          ; do { symbol "+"
               ; e <- expr
               ; return (t+e)
               }
            +++ return t
          }

term = do { f <- factor
          ; do { symbol "*"
               ; t <- term
               ; return (f*t)
               }
            +++ return f
          }

factor = do { symbol "("
               ; e <- expr
               ; symbol ")"
               ; return e
               }
            +++ natural

------------------------------------------------------------------------
-- expr' is a parser for 
--    E ::= T + E | T
--    T ::= F * T | F
--    F ::= Num | (E)
-- it constructs a parse tree for the expression while parsing it.
-- Note that + and * are treated right-associatively.
expr', term', factor' :: Parser Exp
expr' = do { t <- term'
           ; do { symbol "+"
                ; e <- expr'
                ; return (Add t e)
                }
             +++ 
             do { symbol "-"
                ; e <- expr'
                ; return (Sub t e)
                }
             +++
             return t
           }

term' = do { f <- factor'
           ; do { symbol "*"
                ; t <- term'
                ; return (Mult f t)
                }
             +++ 
             do { symbol "/"
                ; t <- term'
                ; return (Div f t)
                }
            +++
            return f
          }
factor' = do { symbol "("
             ; e <- expr'
             ; symbol ")"
             ; return e
             }
          +++ 
          do { n <- natural
             ; return (Num n)
             }
    

-- When you think you have things working, try:
--   quickCheck prop_parse1 
prop_parse1 e = e==t
    where
      [(t,_)] = parse expr' (showExprr e) 

------------------------------------------------------------------------

leftExpr, leftTerm, leftFactor :: Parser Exp
leftExpr = pleaseFix


-- leftTerm is a parser for the left-associtive grammar
--   T ::= T * F | F
--   F ::= Num
-- E.g., parse leftTerm "1*2*3*4" produces
-- [(Mult (Mult (Mult (Num 1) (Num 2)) (Num 3)) (Num 4)),"")].
-- (Mult (Mult (Mult (Num 1) (Num 2)) (Num 3)) (Num 4)) has the parse tree:
--            *
--          /   \
--         *     4
--       /   \
--      *     3
--    /   \
--   1     2

leftTerm = do { tl <- leftFactor
              ; (fromLeft tl)
              }
    where
      fromLeft tl
          = do { symbol "*";
               ; tr <- leftFactor
               ; fromLeft (Mult tl tr)
               }
            +++ 
            return tl

leftFactor = do { symbol "("
                ; e <- leftTerm -- when you define leftExpr, change this line
                ; symbol ")"
                ; return e
                }
             +++
             do { n <- natural
                ; return (Num n)
                }
 

-- When you think you have things working, try:
--   quickCheck prop_parse2
prop_parse2 e = e==t
    where
      [(t,_)] = parse leftExpr (showExprl e) 


-- The following may be useful for debugging while developing.
-- Right now, simplify changes all the Add's, Sub's, and Div's in an
-- Exp to Mult's.  So if your parser handles only Mult's and Num's,
-- prop_simp first simplifies the random expression e and tests the
-- parser leftTerm on the simplified expression.  As your parser
-- gets fancier (e.g., it can handle Num's, Mult's, and Div's), then
-- you can revise simplify (e.g., Div's don't get changed to
-- Mult's).

simplify (Sub e1 e2)  = Mult (simplify e1) (simplify e2)
simplify (Div e1 e2)  = Mult (simplify e1) (simplify e2)
-- simplify (Div e1 e2)  = Div (simplify e1) (simplify e2)
simplify (Mult e1 e2) = Mult (simplify e1) (simplify e2)
simplify (Add e1 e2)  = Mult (simplify e1) (simplify e2)
simplify (Num n)      = Num n

prop_simp e 
    = e'==t
      where
        e' = simplify e
        [(t,_)] = parse leftTerm (showExprl e') 


