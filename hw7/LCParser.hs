module LCParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Test.QuickCheck

import LC2

{- THE GRAMMAR:
     Loc    ::=  x0 | x1 | x2 | ...
     
     Expr   ::=  Term {{ + | - } Term}^*
     Term   ::=  Factor {* Factor}^*
     Factor ::=  (Expr) | Num | val(Loc)

     Bexpr  ::=  ff | tt | Expr Cop Expr | (Bexpr) | not Bexpr
     Cop    ::=  == | /= | < | <= | > | >=
  
     Cmd    ::= skip
              | { } 
              | { Cmd [; Cmd]^* }
              | Loc := Expr
              | if Bexpr then Cmd else Cmd
              | while Bexpr do Cmd
              | for Loc do Cmd
-}

---------------------------------------------------------------------------
-- The lexer
---------------------------------------------------------------------------
langDef = emptyDef 
          { P.reservedNames   
                = ["tt","ff","skip","if","then","else","while","do",
                   "val","for","not"]
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
bexpr     = (try (parens bexpr)) <|> ttExpr <|> ffExpr <|> notExpr <|> compExpr
ttExpr    = do { reserved "tt"; return (BConst True) }
ffExpr    = do { reserved "ff"; return (BConst False) }
notExpr   = do { reserved "not"
               ; be <- bexpr
               ; return (Not be)
               }
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


-- statements/commands
stm       = do { whiteSpace
               ; skipStm <|> ifStm <|> whileStm <|> seqStm 
                 <|> forStm <|> assgnStm
               }

skipStm   = do { reserved "skip" ; return Skip }

ifStm     = do { reserved "if"
               ; tst <- bexpr
               ; reserved "then"
               ; thenStm <- stm
               ; reserved "else"
               ; elseStm <- stm
               ; return (If tst thenStm elseStm)
               }

whileStm  = do { reserved "while"
               ; tst <- bexpr
               ; reserved "do"
               ; body <- stm
               ; return (While tst body)
               }

forStm    = do { reserved "for"
               ; loc <- location
               ; reserved "do"
               ; body <- stm
               ; return (For loc body)
               }

           -- seq ::= { stm (; stm)^* }
seqStm    = do { ss <- braces(semiSep stm)
               ; if null ss then return Skip else return (foldr1 Seq ss) 
               }

assgnStm =  do { n <- location
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
cparse inp = case (parse stm "Stm" inp) of
             Left  m   -> error (show m)
             Right inp -> inp

-- QuickCheck property, try
--    quickCheck prop_lcp
prop_lcp cmd = (cmd == cparse (show cmd))

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