-------------------------------------------------------------------------- 
--									--
--	BuildNfa.hs							--
--									--
--	Building an NFA to recognise a regular expression.		--
--									--
--	Regular expressions are defined in regexp, and the type of	--
--	NFAs in nfa_types. The implementation of sets used is in	--
--	sets. 								--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--          Munged by Jim Royer 2015                                    --
--									--
-------------------------------------------------------------------------- 

module BuildNfa2 where

import NfaTypes
import RegExp2
import Matches2
import Data.Set (Set)
import qualified Data.Set as S
import ImplementNfa
import DrawNfa
import Test.QuickCheck


-------------------------------------------------------------------------- 
--									--
--	Here we build NFAs of type (Nfa Int) to recognise regular	--
--	expressions.							--
--									--
--	We define a series of conbinators for these numeric NFAs, so 	--
--	as to build the results by recursion on the structure of the	--
--	regular expression, an object of type regexp.			--
--									--
-------------------------------------------------------------------------- 


build :: Reg -> Nfa Int
build Epsilon      = NFA
	             (S.fromList [0 .. 1])
	             (S.singleton  (Emove 0 1))
	             0
	             (S.singleton  1)
build (Literal c)  = NFA
	             (S.fromList [0 .. 1])
	             (S.singleton  (Move 0 c 1))
	             0
	             (S.singleton  1)
build (Or r1 r2)   = m_or (build r1) (build r2)
build (Opt r1)     = m_star (build r1)
build (Then r1 r2) = m_then (build r1) (build r2)
build (Star r)     = m_star (build r)
build (Plus r)     = m_then (build r1) (build r2)
-------------------------------------------------------------------------- 
-- Problem 5(b): Add cases for Opt and Plus
build (Opt r)  
    = error ("Translation for " ++ (printRE (Opt r)) ++ " not yet implemented!")
build (Plus r)  
    = error ("Translation for " ++ (printRE (Plus r)) ++ " not yet implemented!")


-------------------------------------------------------------------------- 
--									--
--	Combinators for machines, called by build.			--
--									--
-------------------------------------------------------------------------- 

m_or :: Nfa Int -> Nfa Int -> Nfa Int
m_or (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA
      (S.unions [states1',states2',newstates])
      (S.unions [moves1' ,moves2' ,newmoves] )
      0
      (S.singleton  (m1+m2+1))
    where
      m1        = S.size states1
      m2        = S.size states2
      states1'  = S.map (renumber 1) states1
      states2'  = S.map (renumber (m1+1)) states2
      newstates = S.fromList [0,(m1+m2+1)]
      moves1'   = S.map (renumber_move 1) moves1
      moves2'   = S.map (renumber_move (m1+1)) moves2
      newmoves  = S.fromList [Emove 0 1, Emove 0 (m1+1),
                              Emove m1 (m1+m2+1), Emove (m1+m2) (m1+m2+1) ]

m_then :: Nfa Int -> Nfa Int -> Nfa Int
m_then (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA
      (S.union states1 states2')
      (S.union moves1 moves2')
      start1
      finish2'
    where
      states2' = S.map (renumber k) states2
      moves2'  = S.map (renumber_move k) moves2
      finish2' = S.map (renumber k) finish2
      k        = S.size states1 - 1

m_star :: Nfa Int -> Nfa Int 
m_star (NFA states moves start finish)
    = NFA
      (S.union states' newstates)
      (S.union moves'  newmoves)
      0
      (S.singleton  (m+1))
    where
      m         = S.size states
      states'   = S.map (renumber 1) states
      newstates = S.fromList [ 0 , m+1 ]
      moves'    = S.map (renumber_move 1) moves
      newmoves  = S.fromList [Emove 0 1, Emove m 1, Emove 0 (m+1), 
                              Emove m (m+1)]

-------------------------------------------------------------------------- 
--	Auxilliary functions used in the definition of NFAs from	--
--	regular expressions.						--
-------------------------------------------------------------------------- 

renumber :: Int -> Int -> Int
renumber n st = n + st

renumber_move :: Int -> Move Int -> Move Int
renumber_move k (Move s1 c s2)
      = Move (renumber k s1) c (renumber k s2)
renumber_move k (Emove s1 s2)
      = Emove (renumber k s1) (renumber k s2)


-- Try: quickCheck prop_equivB
prop_equivB :: Reg -> TestStr -> Bool
prop_equivB re (TS str)
    = (matches re str)==(accepts (build re) str)