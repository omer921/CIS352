-- The following should work on a Mac with graphviz installed. 
-- See http://www.graphviz.org/ for details of installing graphviz.

module DrawNfa where

import System.Process
import System.IO
import NfaTypes
import NfaMisc
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (elemIndex)

------------------------------------------------------------------------
-- dump str 
--   writes the temp file /tmp/jim.gv with contents str
--   then opens it

dump str = do { withFile "/tmp/graph.gv" WriteMode action
              ; system "open /tmp/graph.gv"
              }
    where action handle = hPutStrLn handle str 

------------------------------------------------------------------------

drawNfa nfa = dump $ start  ++ draw nfa ++ end
    where
      start = unlines["digraph g {\n",
                      "\t//layout=\"circo\";",
                      "\t// layout=\"fdp\";",
                      "\t// layout=\"twopi\";",
                      "\t// nodesep = 12;",
                      "\trankdir=LR //LR|RL|BT|TB"
                     ]
      end   = "}\n"

-- draw nfa = reduces nfa to the body of a GraphViz program
--    states may be any showable type. 
draw :: (Ord a, Show a) => Nfa a -> String
draw (NFA sts mvs strt fin) 
    = unlines (nodes ++ startNode ++ finNodes ++ edges)
    where 
      --    simpleminded and dirty
      states     = S.toList sts
      stateNum s = maybe 0 id (elemIndex s states)
      nodes      = map labNode states
      labNode s  = '\t':show (stateNum s) ++ "[label=\"" ++ show s ++ "\"];"
      startNode  = ['\t':show (stateNum strt) ++ "[shape=diamond];"]
      finNodes   = map finLabel (S.toList fin)
      finLabel s = '\t':show (stateNum s) ++ "[style=filled]; "
      edges      = map labEdge (S.toList mvs)
      labEdge (Emove s1 s2) 
                 = labEdge (Move s1 '@' s2)
      labEdge (Move s1 c s2) 
                 = '\t':show (stateNum s1) ++ " -> " 
                     ++ show (stateNum s2) ++ " [label=\""++[c]++"\"];"


