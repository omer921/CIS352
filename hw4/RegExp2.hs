
-------------------------------------------------------------------------- 
--									--
--	RegExp.hs							--
--									--
--	A type of regular expressions.					--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module RegExp2 where

import Test.QuickCheck
import Control.Monad( liftM, liftM2 )

data Reg = Empty
         | Epsilon 
	 | Literal Char 
	 | Or Reg Reg 
	 | Then Reg Reg 
	 | Star Reg
         | Opt Reg           
         | Plus Reg          
           deriving Eq

-------------------------------------------------------------------------- 
--	Definitions of ? and +						--
-------------------------------------------------------------------------- 


--opt,plus :: Reg -> Reg          -- Leave this commented out
--opt  re = Or re Epsilon         -- Leave this commented out
--plus re = Then (Star re) re     -- Leave this commented out


-------------------------------------------------------------------------- 
--	Expanding a character range into a regular expression.		--
--									--
--	range 'a' 'c'							--
--	  = Or (Literal 'a') (Or (Literal 'b') (Literal 'c'))		--
-------------------------------------------------------------------------- 

rangeChar :: Char -> Char -> Reg
rangeChar c1 c2
      = foldr1 Or (map Literal [c1 .. c2])

-------------------------------------------------------------------------- 
--	Examples							--
-------------------------------------------------------------------------- 

a       = Literal 'a'
b       = Literal 'b'
bang    = Literal '!'

rex1    = a `Or` (a `Then` b)
rex2    = (a `Then` b) `Or` (Epsilon `Or` (Star a))

regexp0 = Then b (Then (Star regexp2) a)
regexp1 = Then a (Then (Star regexp2) b)
regexp2 = Or (Then a b) (Then b a)

sheep   = Then b (Then (Then a (Star a)) bang)

-------------------------------------------------------------------------- 
--	Which literals occur in a regular expression?			--
-------------------------------------------------------------------------- 
 
literals :: Reg -> [Char]

literals Empty        = []
literals Epsilon      = []
literals (Literal ch) = [ch]
literals (Or r1 r2)   = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r)     = literals r
literals (Opt r)      = literals r   
literals (Plus r)     = literals r   

-------------------------------------------------------------------------- 
--	Pretty printing a regular expression.				--
--									--
--	@ is used instead for the epsilon character.			--
-------------------------------------------------------------------------- 

instance Show Reg where
  show = printRE

printRE :: Reg -> [Char]

printRE Empty   = "_"
printRE Epsilon = "@"
printRE (Literal ch) = [ch]
printRE (Or r1 r2) = "(" ++ printRE r1 ++ "|" ++ printRE r2 ++ ")"
printRE (Then r1 r2) = "(" ++ printRE r1 ++ printRE r2 ++ ")"
printRE (Star r) = "(" ++ printRE r ++")*"
printRE (Opt r) = "(" ++ printRE r ++ "?)"   
printRE (Plus r) = "(" ++ printRE r ++")+"   

-- builds a random Reg over the alphabet {'a','b','c','d'}
instance Arbitrary Reg where
    arbitrary  =  sized regexp
        where
          regexp n | n <= 0     =  atom
                   | otherwise  =  oneof [ atom
                                         , liftM2 Or   subform subform
                                         , liftM2 Then subform subform
                                         , liftM  Star subform
                                         , liftM  Opt  subform
                                         , liftM  Plus subform
                                         ]
                   where
                     atom = oneof [liftM Literal (elements "abcd"),
                                         elements [Epsilon]]
                     subform  =  regexp (n `div` 2)

data TestStr = TS String
               deriving (Show,Eq)

-- builds a random string (of length <= 15) over the 
-- alphabet {'a','b','c','d'}
instance Arbitrary TestStr where
    arbitrary = do { str <- listOf (elements "abcd")
                   ; return (TS (take 15 str))
                   }

