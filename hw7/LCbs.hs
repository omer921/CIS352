------------------------------------------------------------------------
-- A Big-step interpreter for LC2
-- Version 2 (Sun Mar  1 20:36:54 EST 2015)

import LC2 
import State
import LCParser
import Test.QuickCheck
------------------------------------------------------------------------

type AConfig = (AExp,State)     -- Integer configs
type BConfig = (BExp,State)     -- Boolean configs
type CConfig = (Command,State)  -- Command configs

-- evaluator for integer expressions 
evalA :: AConfig -> AConfig
evalA (AConst v,s)              = (AConst v,s)
evalA (Val loc,s)               = (AConst (fetch s loc),s)
evalA (ABin iop ie1 ie2,s)      = (AConst v,s2)
    where (AConst v1,s1) = evalA (ie1,s)
          (AConst v2,s2) = evalA (ie2,s1)
          v              = aApply iop v1 v2

-- evaluator for boolean expressions 
evalB :: BConfig -> BConfig
evalB (BConst v,s)              = (BConst v,s)           
evalB (Not be,s)                = (BConst (not v),s)
    where (BConst v,s)   = evalB (be,s)
evalB (Compare cop ie1 ie2,s)   = (BConst v,s2)
    where (AConst v1,s1) = evalA (ie1,s)
          (AConst v2,s2) = evalA (ie2,s1)
          v              = bApply cop v1 v2

-- evaluator for commands
evalC :: CConfig -> CConfig
evalC (Skip,s)                  = (Skip,s)

evalC (Set loc ie,s)            = (Skip,set s' loc n)
    where (AConst n,s') = evalA (ie,s)

evalC (Seq cmd1 cmd2,s)         = (Skip,s2)
    where (Skip,s1) = evalC (cmd1,s)
          (Skip,s2) = evalC (cmd2,s1)

evalC (If be cmd1 cmd2,s)       = evalC (e,s')
    where (BConst v,s') = evalB (be,s)
          e             = if v then cmd1 else cmd2
          
evalC (While be cmd,s)          = evalC (cmd',s')
    where (BConst v,s') = evalB (be,s)
          cmd'          = if v then (Seq cmd (While be cmd)) else Skip

evalC (For loc cmd,s) 
    | (fetch s loc==0)          = (Skip,s)
    | otherwise                 = evalC (For loc cmd,s2)
    where 
      (Skip,s1) = evalC (cmd,s)
      s2        = set s1 loc ((fetch s1 loc)-1)

{--}

runI text = evalA (aparse text,state0)
runB text = evalB (bparse text,state0)
runC text = evalC (cparse text,state0)


state0 = load empty [0,0,0,0,0]

ie0 = "4"
ie1 = "3+8"
ie2 = "(1+2)*(3+2*(10-2))"

c1 = "{ x1:=12; skip; x2:=9 }"
c2 = "{ x1:=12; skip; x2:=9; x2:=val(x2)-1 }"
c3 = "if (val(x2)==0) then x0:=10 else x0:=20"
c4 = "if not(x2=0) then x0:=10 else x0:=20"
c5 = "{ x0:=4; x1:=1; while (val(x0)>0) do { x1:=val(x1)*val(x0); "
     ++ "x0:=(val(x0)-1) } }"

-- c6 through c10 are for Homework testing
c6 = "{ x0:=10; x1:=20; swap x0 x1 }" 
c7 = "if (val(x8)>0) then x0:=10 else x0:=20"
-- three different ways of computing 4!. The variable b should
--   end up with value 24 in each of them.
c8 = "{ x0:=4; x1:=1; while (val(x0)>0) do "
     ++ "{ x1:=val(x1)*val(x0); x0:=(val(x0)-1) } }"
c9 = "{ x0 := 3; for x0 do skip }"
c10 = "{ x0 := 4; x1 := 1; for x0 do x1:=val(x1)*val(x0) }"



