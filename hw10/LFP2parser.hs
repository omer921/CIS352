module LFP2parser where
-- last updated Sun Apr 15 22:18:12 EDT 2012
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Test.QuickCheck

import LFP2

{- THE GRAMMAR:
   Expr ::= Num | tt | ff | (AOp Expr Expr) | (COp Expr Expr)
          | if Expr then Expr else Expr | while Expr do Expr 
          | skip | { Expr (; Expr)^* } | val(LExp) | LExp := Expr
          | Var | fn Var => Expr |let Var = Expr in Expr | (Expr Expr^+)     

   LExp   ::= X0 | X1 | X2 | ...
   AOp    ::= + | - | *  
   COp    ::= == | /= | < | <= | > | >= 
   Var    ::= (a|...|z)^+
-}

---------------------------------------------------------------------------
-- The lexer
---------------------------------------------------------------------------
langDef = emptyDef 
          { P.reservedNames   
                = ["tt","ff","skip","if","then","else","while","do"
                   ,"val","fn","let","in","return","print"]
          , P.reservedOpNames 
                = [ "=", "==","/=","<","<=",">",">=","+","-","*",":=","=>"]
          , P.identStart  = letter
          , P.identLetter = letter
          }
lexer  = P.makeTokenParser langDef

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- direct imports from Parsec's token parser library
braces     = P.braces     lexer
identifier = P.identifier lexer
integer    = P.integer    lexer
natural    = P.natural    lexer
parens     = P.parens     lexer    
reserved   = P.reserved   lexer    
reservedOp = P.reservedOp lexer
semiSep    = P.semiSep    lexer
commaSep   = P.commaSep   lexer
whiteSpace = P.whiteSpace lexer

-- locations
location   = do { try (char 'X'); n <- natural; return $ Loc (fromIntegral n) }

-- variables
varExp     = do { x <- identifier; return $ Var x }

-- arithmetic expressions
expr  = choice [ numExp,   ttExp,    ffExp,    valExp,  assgnExp, 
                 ifExp,    skipExp,  whileExp, seqExp,  lamExp,
                 try aExp, try bExp, letExp, appExp,   varExp, 
                 returnExp, printExp
               ]

numExp     = do { n <- integer;    return $ AConst (fromIntegral n) }
ttExp      = do { reserved "tt"; return (BConst True) }
ffExp      = do { reserved "ff"; return (BConst False) }
valExp     = do { reserved "val"; loc <- parens(location); return $ Val loc }
assgnExp   = do { n <- location
                ; reservedOp ":="
                ; rhs <- expr 
                ; return $ Set n rhs
                }

ifExp      = do { reserved "if"
                ; tst <- expr
                ; reserved "then"
                ; thenExp <- expr
                ; reserved "else"
                ; elseExp <- expr
                ; return $ If tst thenExp elseExp
                }

skipExp    = do { reserved "skip" ; return Skip }

whileExp   = do { reserved "while"
                ; tst <- expr
                ; reserved "do"
                ; body <- expr
                ; return $ While tst body
                }

seqExp     = do { ss <- braces(semiSep expr)
                ; if null ss then return Skip else return $ foldr1 Seq ss 
                }

aExp       = parens aexp
    where  aexp = do { op <- iopExp
                     ; e1 <- expr
                     ; e2 <- expr
                     ; return $ ABin op e1 e2
                     }

iopExp      = choice [ oper "+" Plus, oper "-" Minus, oper "*" Times]
    where oper str op = do { reservedOp str; return op }

bExp       = parens bexp
    where bexp = do { op <- copExp
                    ; e1 <- expr
                    ; e2 <- expr
                    ; return $ Compare op e1 e2
                    }

copExp     = choice [ coper "==" Eq  , coper "/=" Neq , 
                      coper "<=" Leq , coper ">=" Geq , 
                      coper "<" Lt   , coper ">" Gt   ]   
    where coper st op = do { reservedOp st; return op }


appExp     =  do { es <- parens(many1 expr)
                 ; case es of 
                     [e] -> return e
                     es' -> return $ foldl1 App es'
                 }

lamExp     = do { reserved "fn"
                ; x <- identifier
                ; reservedOp "=>"
                ; body <- expr
                ; return $ Fun x body
                }

letExp      = do { reserved "let"
                 ; x <- identifier
                 ; reservedOp "="
                 ; e0 <- expr
                 ; reserved "in"
                 ; e1 <- expr
                 ; return $ Let x e0 e1
                 }

returnExp   = do { reserved "return"
                 ; e <- expr
                 ; return $ Return e
                 } 

printExp    = do { reserved "print"
                 ; es <- parens(commaSep expr)
                 ; return $ Print es
                 }

---------------------------------------------------------------------------
-- top level functions

-- (eparse inp) = the parse of an LFC expression
eparse inp = case (parse (do { whiteSpace; expr}) "Expr" inp) of
               Left  m   -> error (show m)
               Right inp -> inp

---------------------------------------------------------------------------

