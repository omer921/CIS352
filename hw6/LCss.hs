------------------------------------------------------------------------
-- LC, Small-step semantic interpreter
-- Authors:  Forgot to add my name, please subtract 5pts. 
-- Version 3: 15 Feb 2015

import LC 
import State
import LCParser
import Test.QuickCheck
import LCCEK (cekSteps)

fixMe = error "Please fill in a definition"

------------------------------------------------------------------------

data Phrase = A AExp | B BExp | C Command deriving (Show)

type Config = (Phrase,State) 

-- Integer expressions
step (A (ABin iop (AConst n1) (AConst n2)),s) 
    = (A (AConst n),s)
      where n = aApply iop n1 n2
step (A (ABin iop (AConst n1) ae2),s) 
    = (A (ABin iop (AConst n1) ae2'),s')
      where (A ae2',s') = step (A ae2,s)
step (A (ABin iop ae1 ae2),s) 
    = (A (ABin iop ae1' ae2),s')
      where (A ae1',s') = step (A ae1,s)    
step (A (Val loc),s)            
    = (A (AConst (fetch s loc)),s)

-- Boolean expressions
step (B (Compare cop (AConst n1) (AConst n2)),s) =  (B (BConst n),s)
      where n = bApply cop n1 n2
step (B (Compare cop (AConst n1) ae2),s)         = (B (Compare cop (AConst n1) ae2'),s')
      where (A ae2',s') = step (A ae2,s)
step (B (Compare cop ae1 ae2),s)                 = (B (Compare cop ae1' ae2),s')
      where (A ae1',s') = step (A ae1,s)
-- Set 
step (C (Set loc (AConst n)),s) = (C Skip,set s loc n)
step (C (Set loc ae),s)                          = (C (Set loc ae'), s')
     where (A ae', s') = step (A ae, s)

-- sequencing 
step (C (Seq Skip c2),s)                         = (C c2, s)
step (C (Seq c1 c2),s)                           = (C c1', s')
     where (C c1', s') = step (C (Seq Skip c1),s)
-- if
step (C (If (BConst True)  c1 c2),s)             = step (C c1, s)
step (C (If (BConst False) c1 c2),s)             = step (C c2, s)
step (C (If be c1 c2),s)                         = (C (If be' c1 c2), s')
     where (C (If be' c1 c2), s') = step (C (If be c1 c2), s)
      
-- while
step (C (While be c),s) = (C (If be (Seq c (While be c)) Skip),s)

-- terminal c, tests if c is a terminal config.
terminal (A (AConst _),_) = True
terminal (B (BConst _),_) = True
terminal (C Skip      ,_) = True
terminal _                = False

-- steps c = the result of iterating step until a terminal config appears
steps  c = if terminal c then c   else steps (step c)

-- steps' c = the same as steps **except** we return the entire
--            sequence of configurations
steps' c = if terminal c then [c] else c:steps' (step c)

-- showConfig c, prints the config c
showConfig n (phr,s)    = putStr $ "Step: " ++ show n 
              ++ "\n " ++ take 72 (show phr)
              ++ "\n " ++ concatMap f (dump s)
      where f (l,n) = "s["++ show l ++ "]=" ++ show n ++ "   "

-- Try: steps (A ae, state4)
-- When you get things working, try: stepRun fact state4


-- Run: quickCheck ss_prop
--   This checks that your big-step evaluator gives the same final
--   states as the CEK evaluator on the same commands. 
--   N.B. The test commands have no while-loops.

ss_prop :: Command -> Bool
ss_prop cmd = s1==s2
    where (_,s1) = steps (C cmd,state4)
          s2     = cekSteps cmd state4

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Some more testing tools
------------------------------------------------------------------------
------------------------------------------------------------------------

-- Source version of sample expressions and commands

-- integer expressions
ae0 = "0"
ae1 = "1+2"
ae2 = "val(x1)+2"
ae3 = "(val(x0)-3)*(7-val(x1))"

-- boolean expressions
be0 = "tt"
be1 = "9>8"
be2 = "9<8"
be3 = "(val(x0)-3) == (7-val(x1))"

-- commands 
cmd0 = "skip"
cmd1 = "x0 := 100"
cmd2 = "{ x0 := 100; x1 := val(x0)-1 }"
cmd3 = "{ x0 := (-2); x3 := ((-3)+val(x1)) }"
cmd4 = "if tt then x0 := 100 else x0 := 200"
cmd5 = "if ff then x0 := 100 else x0 := 200"
cmd6 = "if (val(x0)>0) then x0 := val(x0)-1 else x1 := val(x1)+3}"
cmd7 = "while (val(x0)>0) do { x0 := val(x0)-1; x1 := val(x1)+3 }"
-- cmd8 = "for 3 do x0 := val(x0) + 10"

-- a sample state
state0 = load empty [1,0,3] -- So: val(x0)=1, val(x1)=0, val(x2)=3

------------------------------------------------------------------------
-- For running commands and seeing the final configuration
------------------------------------------------------------------------
-- run cmd state
--   prints the final configuration of the run of (C cmd, state)
run :: Command -> State -> IO ()
run cmd state = do showConfig (-1) (steps (C cmd,state))
                   putStrLn ""

-- run' inp state
--   parses inp to get a command cmd
--   then prints the final configuration of the run of (C cmd, state)
-- Try: (run cmd0 state0)
run' :: String -> State -> IO ()
run' inp state = run (cparse inp) state

------------------------------------------------------------------------
-- For running commands step by step
------------------------------------------------------------------------

-- stepRun cmd state
--   prints the run of the small step semantics started
--   on initial configuration (C cmd, state)
stepRun :: Command -> State -> IO ()
stepRun cmd state = stepByStep 0 (steps' (C cmd,state))

-- stepRun' inp state
--   parses inp to get a command cmd, then
--   prints the run of the small step semantics started
--   on initial configuration (C cmd, state)
-- Try: (stepRun' cmd0 state0)
stepRun'  inp state = stepRun  (cparse inp) state

-- stepRunA ae state
--   prints the run of the small step semantics started
--   on initial configuration (A ae, state)
stepRunA :: AExp -> State -> IO ()
stepRunA ae state = stepByStep 0 (steps' (A ae,state))

-- Try: (stepRunA' ae3 state0)
stepRunA' inp state = stepRunA (aparse inp) state

-- stepRunB be state
--   prints the run of the small step semantics started
--   on initial configuration (B be, state)
stepRunB :: BExp -> State -> IO ()
stepRunB be state = stepByStep 0 (steps' (B be,state))

-- Try: (stepRunB' be3 state0)
stepRunB' inp state = stepRunB (bparse inp) state

-- Print out steps one at a time
stepByStep _ [] = putStr ""
stepByStep n (c:cs)
    = do { showConfig n c
         ; putStr "  <tap return>"
         ; getChar
         ; stepByStep (n+1) cs 
         }