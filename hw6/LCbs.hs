------------------------------------------------------------------------
-- A Big-step interpreter for LC
-- Version 3: 15 Feb 2015

import LC 
import State
import LCParser
import LCCEK (cekSteps)
import Test.QuickCheck
------------------------------------------------------------------------
pleaseFixMe = error "Please fill in a definition"

type AConfig = (AExp,State)     -- Arithmetic configs
type BConfig = (BExp,State)     -- Boolean configs
type CConfig = (Command,State)  -- Command configs

-- evaluator for integer expressions 
evalA :: AConfig -> AConfig
evalA (AConst v,s)              = (AConst v,s)
evalA (Val loc,s)               = (AConst (fetch s loc),s)
evalA (ABin aop ae1 ae2,s)      = (AConst v,s2)
    where (AConst v1,s1) = evalA (ae1,s)
          (AConst v2,s2) = evalA (ae2,s1)
          v              = aApply aop v1 v2

-- evaluator for boolean expressions 
evalB :: BConfig -> BConfig
evalB (BConst v,s)              = (BConst v,s)           
evalB (Compare cop ae1 ae2,s)   = (BConst v,s2)
    where (AConst v1,s1) = evalA (ae1,s)
          (AConst v2,s2) = evalA (ae2,s1)
          v              = bApply cop v1 v2

-- evaluator for commands
evalC :: CConfig -> CConfig
evalC (Skip,s)                  = (Skip,s)
evalC (Set loc ae,s)            = (Skip,set s' loc n)
    where (AConst n,s') = evalA (ae,s)
      
evalC (Seq cmd1 cmd2,s)         = (Skip,s2)
    where (Skip,s1) = evalC (cmd1,s)
          (Skip,s2) = evalC (cmd2,s1)
evalC (If be cmd1 cmd2,s)       = evalC (e,s')
    where (BConst v,s') = evalB (be,s)
          e             = if v then cmd1 else cmd2
          
evalC (While be cmd,s)      = evalC (cmd',s')
    where (BConst v,s') = evalB (be,s)
          cmd'          = if v then (Seq cmd (While be cmd)) else Skip

evalC (Repeat cmd be,s)     = evalC (cmd',s')
    where (BConst v,s') = evalB (be,s)
          cmd'          = if v then (Seq cmd (Repeat cmd be)) else Skip


-- Try: evalA (ae,state4)
-- Try: evalC (fact,state4) 

-- Run: quickCheck bs_prop
--   This checks that your big-step evaluator gives the same final
--   states as the CEK evaluator on the same commands. 
--   N.B. The test commands have no while-loops.

bs_prop :: Command -> Bool
bs_prop cmd = s1==s2
    where (_,s1) = evalC (cmd,state4)
          s2     = cekSteps cmd state4

------------------------------------------------------------------------
-- Some more testing tools
------------------------------------------------------------------------

-- (runI inp s) parses inp and then evaluates the resulting
--    integer expression in state s
-- Try: (runI ae0 state4)
runI inp state = evalA (aparse inp, state)
ae0 = "0"
ae1 = "1+2"
ae2 = "val(x1)+2"
ae3 = "(val(x0)-3)*(7-val(x1))"

-- (runB inp s) parses inp and then evaluates the resulting
--    boolean expression in state s
-- Try: (runI be0 state4)
runB inp state = evalB (bparse inp, state)
be0 = "tt"
be1 = "9>8"
be2 = "9<8"
be3 = "(val(x0)-3) == (7-val(x1))"


-- (run inp s) parses inp and then evaluates the resulting
--    command with starting state s
-- Try: (run cmd0 state4)
run inp state = evalC (cparse inp, state)
cmd0 = "skip"
cmd1 = "x0 := 100"
cmd2 = "{ x0 := 100; x1 := val(x0)-1 }"
cmd3 = "{ x0 := (-2); x3 := ((-3)+val(x1)) }"
cmd4 = "if tt then x0 := 100 else x0 := 200"
cmd5 = "if ff then x0 := 100 else x0 := 200"
cmd6 = "if (val(x0)>0) then x0 := val(x0)-1 else x1 := val(x1)+3}"
cmd7 = "while (val(x0)>0) do { x0 := val(x0)-1; x1 := val(x1)+3 }"
cmd8 = "{ x0 := 0; repeat x0 := val(x0) + 10 until (val(x0) > 20) }"