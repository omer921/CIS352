
-------------------------------------------------------------------------- 
--									--
--	NfaLib.hs							--
--									--
--	Useful functions used in the implementation of an NFA and	--
--	the conversion of an NFA to a DFA.				--
--	Therefore used in ImplementNfa and NfaToFDfa.			--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module NfaLib where

import RegExp2
import Data.Set (Set)
import qualified Data.Set as S
import NfaTypes
import Data.List hiding (union)

-------------------------------------------------------------------------- 
--									--
--	The epsilon closure of a set of states in an NFA. This is 	--
--	found by finding the limit of the function which adds to a	--
--	set all those states accessible by a single epsilon move.	--
--	The limit is found using setlimit.				--
--									--
-------------------------------------------------------------------------- 

closure :: Ord a => Nfa a -> Set a -> Set a

closure (NFA states moves start term)
      = setlimit add
	where
	add stateset = S.union stateset (S.fromList accessible)
	    where
	      accessible
		  = [ s | x <- S.toList stateset , 
		          (Emove y s) <- S.toList moves ,
			  y==x ]


setlimit :: (Ord a) => (Set a -> Set a) -> Set a -> Set a
setlimit f s = let next = f s
                in if s==next 
                      then s 
                      else setlimit f next


-------------------------------------------------------------------------- 
--									--
--	Onemove finds the set of states accessible from a given set	--
--	by a single move on the given character.			--
--									--
-------------------------------------------------------------------------- 

onemove :: Ord a => Nfa a -> Char -> Set a -> Set a

onemove (NFA states moves start term) c x
      = S.fromList [ s | t <- S.toList x , 
		      Move z d s <- S.toList moves ,
		      z==t , c==d ]

-------------------------------------------------------------------------- 
--									--
--	Onetrans performs one move (by onemove) and then takes the	--
--	epsilon closure of the result.					--
--									--
-------------------------------------------------------------------------- 

onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a

onetrans mach c x = closure mach (onemove mach c x)

-------------------------------------------------------------------------- 
--									--
--	Auxilliary functions.						--
--									--
--	startstate	extracts the start state of a machine.		--
--									--
--	alphabet 	returns the alphabet of the machine, by 	--
--			finding a list of the characters mentioned	--
--			in the Moves.					--
--									--
-------------------------------------------------------------------------- 

startstate :: Nfa a -> a

startstate (NFA states moves start finish) = start

alphabet :: Nfa a -> [Char]

alphabet (NFA s moves st f)
  = nub [ c | Move s c t <- S.toList moves ]

