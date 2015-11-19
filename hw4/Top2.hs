-------------------------------------------------------------------------- 
-- Name:Omer Winrauke							--
--	Top2.hs								--
--									--
--	Top level file in the NFA library.				--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--	as munged by Jim Royer, 2012					--
--									--
-------------------------------------------------------------------------- 

module Top2 where

import BuildNfa2
import ImplementNfa
import NfaLib
import NfaToDfa
import NfaMisc
import NfaTypes
import RegExp2
import Matches2
import DrawNfa


data Nonempty = Empty
         | Epsilon 
	 | Literal Char 
	 | Or Nonempty Nonempty 
	 | Then Nonempty Nonempty 
	 | Star Nonempty
         | Opt Nonempty           
         | Plus Nonempty          
           deriving Eq