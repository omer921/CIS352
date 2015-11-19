-- Omer Winrauke

module Parser1 where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

import AS1

{- THE GRAMMAR:
     Expr   ::=  Aexpr | Aexpr ? Aexpr : Expr
     Aexpr  ::=  Term {{ + | - } Term}^*
     Term   ::=  Factor {{ * | / } Factor}^*
     Factor ::=  Num | (Expr) 

Note that: 1-2-3-4 = (((1-2)-3)-4), 
           1*2+3 = (1*2)+3,
           1+2*3 = 1+(2*3),
           1?2:3?4:5 = ?2:(3?4:5), 
           etc.
-}

---------------------------------------------------------------------------
-- The lexer
---------------------------------------------------------------------------
myDef = emptyDef { P.reservedOpNames = [ "+","-","*","/","?",":"] }
lexer = P.makeTokenParser myDef

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- direct imports from Parsec's token parser library
parens     = P.parens     lexer    
reserved   = P.reserved   lexer    
reservedOp = P.reservedOp lexer
natural    = P.natural    lexer
whiteSpace = P.whiteSpace lexer

-- arithmetic expressions
expr       = do { e <- aexpr
                ; option e (cond e)
                }
cond tst   = do { reservedOp "?"
                ; thenExpr <- aexpr
                ; reservedOp ":"
                ; elseExpr <- expr
                ; return (Cond tst thenExpr elseExpr)
                }

aexpr      = do { whiteSpace
                ; term `chainl1` (plusExpr <|> subExpr)
                }
term       = factor `chainl1` (multExpr <|> divExpr)
factor     = numExpr <|> parens(expr)

plusExpr   = do { reservedOp "+"; return Add }
subExpr    = do { reservedOp "-"; return Sub }
multExpr   = do { reservedOp "*"; return Mult }
divExpr    = do { reservedOp "/"; return Div }
numExpr    = do { n <- natural; return (Num (fromIntegral n)) }

---------------------------------------------------------------------------
-- top level functions

-- (aparse inp) = the parse of an arithmetic expression 
aparse inp = case (parse expr "Aexp" inp) of
               Left  m   -> error (show m)
               Right inp -> inp

---------------------------------------------------------------------------

{- Grammar
   var ::= word
   expr ::= term { + term }^*
   term ::= (expr) | var
-}
