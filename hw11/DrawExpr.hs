-- The following should work on a Mac with graphviz installed. 
-- See http://www.graphviz.org/ for details of installing graphviz.
-- For other systems make apropriate adjustments. 

module DrawExpr where
import Test.QuickCheck
import System.Process
import System.IO
import AS

------------------------------------------------------------------------
-- drawExpr expr 
--    reduces an Expr to a GraphViz graph and then displays this graph.
drawExpr expr = dump $ start ++ draw expr ++ end
    where
      start = "digraph g {\n\trankdir=TB //LR|RL|BT|TB\n"++
              "    node [color=blue,style=\"bold\",fontsize=18];"
      end   = "}\n"

-- the following deals the output of a parser.  Try:
--    dr (parse expr' "2+3*4")
dr ((e,_):_) = drawExpr e

------------------------------------------------------------------------
-- dump str 
--   writes the temp file /tmp/jim.gv with contents str then opens it

dump str = do { withFile "/tmp/jim.gv" WriteMode action
              ; system "open /tmp/jim.gv"
              }
    where action handle = hPutStrLn handle str 

------------------------------------------------------------------------
-- draw expr = reduces expr tree to the body of a GraphViz program
-- draw :: (Ord a, Show a) => Nfa a -> String
draw e = result
    where
      (result,_,_) = (drawx e 0)
      showNode n lab    = "\t"++show n ++ " [label="++lab++"];\n"
      showArrow from to = "\t"++show from ++" -> "++ show to ++";\n"
      drawx :: Exp -> Int -> (String,Int,Int)
      -- drawx e n = (pic,t,n') 
      -- where e is an expression and 
      --       n is the next available node # for the GraphViz graph
      --       pic is the expr tree reduced to a GraphViz commands
      --       t is node number of the top node of the GraphViz graph
      --       n' is the next available node # for the GraphViz graph
      --          after processing e
      drawx (Num k)      n = (showNode n (show k),n,n+1)
      drawx (Add  e1 e2) n = branch e1 e2 n "Add"
      drawx (Sub  e1 e2) n = branch e1 e2 n "Sub"
      drawx (Mult e1 e2) n = branch e1 e2 n "Mult"
      drawx (Div  e1 e2) n = branch e1 e2 n "Div"
      branch e1 e2 n lab
          = (showNode n lab ++ showArrow n nlft ++ showArrow n nrght 
             ++ left ++ right, 
             n,
             n2)
            where 
              (left, nlft, n1) = drawx e1 (n+1)
              (right,nrght,n2) = drawx e2 n1
