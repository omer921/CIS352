
-------------------------------------------------------------------------- 
--									--
--	ImplementNfa.hs							--
--									--
--	Implementing an NFA.						--
--									--
--	Regular expressions are defined in regexp, and the type of	--
--	NFAs in NfaTypes. The implementation of Sets used is in	the	--
--	sets module. The module NfaLib contains functions used by both	--
--	this module and the module NfaToDfa which converts to a 	--
--	deterministic machine.						--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--      and munged by Jim Royer 2012                                    --
--									--
-------------------------------------------------------------------------- 

module ImplementNfa where

import RegExp2
import Data.Set (Set)
import qualified Data.Set as S
import NfaTypes
import NfaLib
import Data.List (tails)


-------------------------------------------------------------------------- 
--									--
--	Trans runs down a string, applying onetrans repeatedly for the	--
--	characters in the string, starting from the start state.	--
--	The result is therefore the set of all states accessible from	--
--	the start state by accepting the items in the string; the	--
--	result can be the empty set, of course.				--
--									--
-------------------------------------------------------------------------- 

trans :: Ord a => Nfa a -> String -> Set a
trans mach str = foldl step startset str
    where
      step set ch = onetrans mach ch set
      startset    = closure mach (S.singleton (startstate mach))

trans' :: Ord a => Nfa a -> [Char] -> [Set a]
trans' mach str = scanl step startset str
    where
      step set ch = onetrans mach ch set
      startset    = closure mach (S.singleton (startstate mach))

accepts :: Ord a => Nfa a -> String -> Bool
accepts mach@(NFA _ _ _ accp) str = S.size(S.intersection last accp)>0 
    where
      last = trans mach str 


-------------------------------------------------------------------------- 
--	Thanks are due to Sven Martin for pointing out the omission 	--
--	of the closure in the definition of startset.			--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Turn the result of trans into printable form.			--
-------------------------------------------------------------------------- 

show_trans :: Nfa Int -> String -> [Char]
show_trans mach str = show (S.toList (trans mach str))

print_trans' :: Nfa Int -> String -> IO ()
print_trans' mach str = mapM_ showStep steps
    where
      steps = zip (trans' mach str) (tails str)
      showStep (states,str) = do { print (S.toList states)
                                 ; putStrLn (str++"\n")
                                 }

