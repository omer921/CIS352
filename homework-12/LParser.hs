-- CIS 352: Homework 12
-- Authors: Barry & Omer 

module LParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Test.QuickCheck

import LL

{- THE GRAMMAR (This is equivalent to the grammar in the handout.)
     LOWER       ::=  all lower case letters
     NOTX        ::=  all lower case letters but x
     UPPER       ::=  all upper case letters

     VAR         ::=  x0 | x1 | ...
     CONST       ::=  NOTX LOWER^*
     INDIV       ::=  VAR | CONST

     RELNAME     ::=  UPPER LOWER^* 
     
     EXPR        ::=  TERM   { + TERM }^*
     TERM        ::=  FACTOR { & FACTOR }^*
     FACTOR      ::=  NEGEXPR | FORALLEXPR | EXISTSEXPR | ATOM
     NEGEXPR     ::=  ~ATOM
     FORALLEXPR  ::=  Forall VAR (EXPR)
     EXISTSEXPR  ::=  Exists VAR (EXPR)
     RELEXPR     ::=  RELNAME[INDIV {,INDIV}^*] 
     ATOM        ::=  RELEXPR | (EXPR)

   Note: There are no set braces (or ``^*'') in the language.  
   ``{'' and ``}'' are grammarical brackets.  E.g., in
     EXPR        ::=  TERM   { + TERM }^*   
   the { }^* is short hand for 
     EXPR        ::=  TERM   TERMS
     TERMS       ::=  + TERM TERMS | espilon
   and similarly in the TERM and RELEXPR rules.
-}

fixMe = error "Please fix me"

---------------------------------------------------------------------------
-- The lexer
---------------------------------------------------------------------------
langDef = emptyDef 
          { P.reservedNames   = ["Forall","Exists"]
          , P.reservedOpNames = [ "+","&","~"]
          , P.caseSensitive   = True
          }
lexer  = P.makeTokenParser langDef

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- direct imports from Parsec's token parser library
brackets   = P.brackets   lexer
identifier = P.identifier lexer
parens     = P.parens     lexer    
commaSep   = P.commaSep   lexer
natural    = P.natural    lexer
reserved   = P.reserved   lexer    
reservedOp = P.reservedOp lexer
whiteSpace = P.whiteSpace lexer
---------------------------------------------------------------------------
-- Samples: 
hNats = do { whiteSpace; brackets(commaSep natural) }
lNats = do { whiteSpace; parens(sepBy natural whiteSpace) }
-- Try: 
--  run hNats "  [1, 32, 99 ] "
--  run lNats "  ( 1 234 97 23 ) " 
---------------------------------------------------------------------------

var        = do { char 'x'; n <- natural; return (Var (fromIntegral n)) }
cons       = do { name <- identifier; return (Const name) }
indiv      = var <|> cons

-- RELNAME ::= UPPER LOWER^*
relName :: Parser String
relName    = do {
				x <- upper;
				xs <- many (lower);
				return (x:xs)
				}

-- ATOM        ::=  RELEXPR | (EXPR)
atom       = relExpr
				<|>
				parens (expr)

-- RELEXPR     ::=  RELNAME[INDIV {,INDIV}^*] | (EXPR)
relExpr    = do { 
				 xs <- relName;
				 args <- brackets (commaSep indiv);
				 return (Pred xs args)
				 }
			<|>
			parens (expr)

-- NEGEXPR     ::=  ~ATOM
negExpr    = do { 
				char '~';
				u <- atom;
				return (Not u)
				}

-- TERM        ::=  FACTOR { & FACTOR }^*
term       = factor `chainl1`
			 do { 
				reservedOp "&";
				return And
			    }

-- FACTOR      ::=  NEGEXPR | FORALLEXPR | EXISTSEXPR | ATOM
factor     = negExpr
			 <|>
			 forAllExpr
			 <|>
			 existExpr
			 <|>
			 atom
			 

-- FORALLEXPR  ::=  Forall VAR (EXPR)
forAllExpr = do { 
				reserved "Forall";
				(Var b) <- var;
				p <- parens expr;
				return (Forall b p)
				} 

-- Note: The abstract syntax of "Forall x12 (R[x12,x3])" is: 
--     (Forall 12 (Pred "R" [Var 12,Var 3]))
-- The first argument of this Forall is 12, which stands for 
-- the variable x12.  In parsing the quantified variable in 
-- such expressions, do something like 
--     (Var n) <- var
-- to parse the quantified variable and to bind the variable's
-- number to n.

-- EXISTSEXPR  ::=  Exists VAR (EXPR)
existExpr  = do { 
				reserved "Exists";
				(Var x) <- var;
				e <- parens expr;
				return (Exists x e)
				}

-- EXPR        ::=  TERM   { + TERM }^*
expr :: Parser Expr
expr       = term `chainl1`
			  do {
				reservedOp "+";
				return Or
				}

---------------------------------------------------------------------------
-- top level test functions

-- eparse inp 
--    = the parse of a logic expression 
eparse inp = case (parse (do { whiteSpace; expr }) "Expr" inp) of
               Left  m   -> error (show m)
               Right inp -> inp


-- QuickCheck property, try
--    quickCheck prop1
prop1 e = (e == eparse (showExpr e))

-- QuickCheck property, try
--    quickCheck prop2
prop2 e = (e == eparse (showExprl e))


run p inp = parse p "" inp

problem = Pred "Teases" [Const "sethra",Const "morrolan"]


------------------------------------------------------------------------
------------------------------------------------------------------------
-- The example worked in class April 14

{- Grammar
   var ::= (letter)^+  (i.e., a string of one or more letters)
   expr ::= term { + term }^*
   term ::= (expr) | var
-}

data Expr1 = Or1 Expr1 Expr1 | Vari String
            deriving (Show,Eq)

expr1 :: Parser Expr1
expr1 = do { whiteSpace  -- eat up initial whitespace
           ; ts <- sepBy1 term1 (reservedOp "+")
           ; return (buildTree ts)
           }

buildTree :: [Expr1] -> Expr1
buildTree [t] = t
buildTree (t:ts) = Or1 t (buildTree ts)

vari :: Parser String
vari = do { v <- many1 letter
          ; whiteSpace  -- eat up trailing whitespace
          ; return v 
          }

term1 :: Parser Expr1
term1 = do { name <- vari ; return (Vari name) }
           <|>
           parens expr1

-- Here is an alternative version of buildTree' (and it is kind of 
--  a hint for the left-associative problem).
buildTree' ts = foldr1 Or1 ts

-- Here are alternative versions of expr1 and term1 that
--   build the tree of Or-expressions recursively.
expr1' :: Parser Expr1
expr1' = do { whiteSpace
            ; t <- term1'
            ; do { reservedOp "+"
                 ; e' <- expr1'
                 ; return (Or1 t e')
                 }
              <|>  -- If there is no "+", t must be the end of the term.
              return t
            }

term1' :: Parser Expr1
term1' = do { name <- vari ; return (Vari name) }
            <|>
            parens expr1'
