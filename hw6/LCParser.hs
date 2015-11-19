-- Version 2, Tue Feb 25 13:15:08 EST 2014
module LCParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Test.QuickCheck

import LC

{- THE GRAMMAR:
     Loc    ::=  x0 | x1 | x2 | ...
     
     Expr   ::=  Term {{ + | - } Term}^*
     Term   ::=  Factor {* Factor}^*
     Factor ::=  (Expr) | Num | val(Loc)

     Bexpr  ::=  ff | tt | Expr Cop Expr | (Bexpr)
     Cop    ::=  == | /= | < | <= | > | >=
  
     Cmd    ::= skip
              | { } 
              | { Cmd [; Cmd]^* }
              | Loc := Expr
              | if Bexpr then Cmd else Cmd
              | while Bexpr do Cmd
-}

---------------------------------------------------------------------------
-- The lexer
---------------------------------------------------------------------------
langDef = emptyDef 
          { P.reservedNames   
                = ["tt","ff","skip","if","then","else","while","do","val",
                   "repeat","until"]      -- ,"for"]

          , P.reservedOpNames 
                = [ "==","/=","<","<=",">",">=","+","-","*",":="]
          }
lexer  = P.makeTokenParser langDef

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- direct imports from Parsec's token parser library
braces     = P.braces     lexer
integer    = P.integer    lexer
natural    = P.natural    lexer
parens     = P.parens     lexer    
reserved   = P.reserved   lexer    
reservedOp = P.reservedOp lexer
semiSep    = P.semiSep    lexer
whiteSpace = P.whiteSpace lexer

-- locations
location   = do { char 'x'; n <- natural; return (fromIntegral n) }

-- arithmetic expressions
expr       = term   `chainl1` (plusExpr <|> subExpr)
term       = factor `chainl1` multExpr
factor     = valExpr <|> numExpr <|> parens(expr)

plusExpr   = do { reservedOp "+";  return (ABin Plus)  }
subExpr    = do { reservedOp "-";  return (ABin Minus) }
multExpr   = do { reservedOp "*";  return (ABin Times) }
numExpr    = do { n <- integer;    return (AConst (fromIntegral n)) }
valExpr    = do { reserved "val"; n <- parens(location); return (Val n) }

-- boolean expressions
bexpr     = (try (parens bexpr)) <|> ttExpr <|> ffExpr <|> compExpr
ttExpr    = do { reserved "tt"; return (BConst True) }
ffExpr    = do { reserved "ff"; return (BConst False) }
compExpr  = do { e1 <- expr
               ; cop <- cOp
               ; e2 <- expr
               ; return (Compare cop e1 e2)
               }

cOp       = do { reservedOp "=="; return Eq }
            <|>
            do { reservedOp "/="; return Neq }
            <|>
            do { reservedOp "<";  return Lt }
            <|>
            do { reservedOp "<="; return Leq }
            <|>
            do { reservedOp ">";  return Gt }
            <|>
            do { reservedOp ">="; return Geq }


{-
cOp         = choice [ coper "==" Eq  , coper "/=" Neq , coper "<" Lt , 
                       coper "<=" Leq , coper ">" Gt  , coper ">=" Geq ]
    where
      coper st op = do { reservedOp st; return op }
-}


-- commands
cmd       = do { whiteSpace
               ; skipCmd <|> ifCmd <|> whileCmd <|> seqCmd <|> assgnCmd
                 <|> repeatCmd
--                 <|> forCmd
               }

skipCmd   = do { reserved "skip" ; return Skip }

ifCmd     = do { reserved "if"
               ; tst <- bexpr
               ; reserved "then"
               ; thenCmd <- cmd
               ; reserved "else"
               ; elseCmd <- cmd
               ; return (If tst thenCmd elseCmd)
               }

whileCmd  = do { reserved "while"
               ; tst <- bexpr
               ; reserved "do"
               ; body <- cmd
               ; return (While tst body)
               }

repeatCmd  = do { reserved "repeat"
               ; body <- cmd
               ; reserved "until"
               ; tst <- bexpr
               ; return (Repeat body tst)
               }

{-
forCmd    = do { reserved "for"
               ; count <- expr
               ; reserved "do"
               ; body <- cmd
               ; return (For count body)
               }
-}
           -- seq ::= { cmd (; cmd)^* }
seqCmd    = do { ss <- braces(semiSep cmd)
               ; if null ss then return Skip else return (foldr1 Seq ss) 
               }

assgnCmd =  do { n <- location
               ; reservedOp ":="
               ; rhs <- expr;
               ; return (Set n rhs)
               }

---------------------------------------------------------------------------
-- top level test functions

-- (aparse inp) = the parse of an arithmetic expression 
aparse inp = case (parse (do { whiteSpace; expr}) "Expr" inp) of
               Left  m   -> error (show m)
               Right inp -> inp

-- (bparse inp) = the parse of a boolean expression 
bparse inp = case (parse (do { whiteSpace; bexpr }) "Bexpr" inp) of
             Left  m   -> error (show m)
             Right inp -> inp

-- (cparse inp) = the parse of a statement 
cparse inp = case (parse cmd "Cmd" inp) of
             Left  m   -> error (show m)
             Right inp -> inp

-- QuickCheck property, try
--    quickCheck prop_lcp
prop_lcp cmd = (cmd == cparse (show cmd))


fact' = "{ x1 := 1; x2 := val(x0); while (val(x2)>0) do { x1 := (val(x1)*val(x2)); x2 := (val(x2)-1) } }"



---------------------------------------------------------------------------

--  S ::= a(&a)^*

{-
data Atree = AnA | Node Atree Atree
             deriving (Show)

sillyl = do { whiteSpace
            ; getA `chainl1` getEt
            }
sillyr = do { whiteSpace
            ; getA `chainr1` getEt
            }

getA  = do { P.symbol "a"; return AnA }
getEt = do { P.symbol "&"; return Node }


-}