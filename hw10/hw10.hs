-- hw10.hs, last updated Wed Apr  1 20:19:27 EDT 2015
-- Omer Winrauke


import LFP2
import LFP2parser
import State
import Data.List (intercalate)
import Debug.Trace (trace)

pleaseFix = error "Please fix me"

type Config = (Expr,State)

showExpEnv e env = "Eval trace: " ++ (show e) 
                   ++ "\n  env = " ++ (take 70 (show env))

------------------------------------------------------------------------
-- Eval: a big step semantics for LFP under the environment model

eval :: Env -> Config -> Config
-- Uncomment the following line to trace eval
-- eval env (e,s) | trace (showExpEnv e env) False = undefined

-- Constants and such
eval env (AConst v,s)            = (AConst v,s)
eval env (BConst v,s)            = (BConst v,s)
eval env (Loc loc,s)             = (Loc loc,s)        
eval env (Skip,s)                = (Skip,s)
eval env (Closure e env',s)      = error ("Attempt to evaluate: "
                                          ++ show (Closure e env'))
-- Arithmetic and comparison expressions
eval env (Val (Loc loc),s)       = (AConst (fetch s loc),s)
eval env (ABin iop e1 e2,s)      = (AConst (aApply iop v1 v2),s'')
    where
      (AConst v1,s')  = eval env (e1,s)
      (AConst v2,s'') = eval env (e2,s')
eval env (Compare cop e1 e2,s)   = (BConst (bApply cop v1 v2),s'')
    where
      (AConst v1,s')  = eval env (e1,s)
      (AConst v2,s'') = eval env (e2,s')
-- Commands 
eval env (Set le ie,s)           = (Skip,set s' loc n)
    where (AConst n,s') = eval env (ie,s)
          (Loc loc,s'') = eval env (le,s')
eval env (Seq cmd1 cmd2,s)       = (v2,s2) -- Note changes
    where (v1,s1) = eval env (cmd1,s )
          (v2,s2) = eval env (cmd2,s1)    
eval env (If be cmd1 cmd2,s)     = eval env (e,s')
    where (BConst v,s') = eval env (be,s)
          e = if v then cmd1 else cmd2
eval env (While be cmd,s)        = eval env (cmd',s')
    where (BConst v,s') = eval env (be,s)
          cmd' = if v then (Seq cmd (While be cmd)) else Skip
eval env (Print es,s) = trace str (Skip,s) 
    where (vs,s') = evals es s
          evals [] s = ([],s)
          evals (e:es) s0   = (v:vs,s2)
              where (v,s1)  = eval env (e,s0)
                    (vs,s2) = evals es s1
          str = 
              -- "\ESC[41;37m "++  -- comment out this line for no color
              "PRINT: "++intercalate "  " (map show vs) 
              -- ++ " \ESC[0m"     -- comment out this line for no color
              ++ "\n"
eval env (Return e,s) = (v,s')
    where (v,s') = eval env (e,s)
-- Let, we use the fact that
--     (let x = e1 in e2) == (Fun x e2) applied to e2
eval env (Let x e0 e1,s) = eval env (App (Fun x e1) e0,s)

------------------------------------------------------------------------
------------------------------------------------------------------------

-- Call-by-Value, lexical scoping (comment out for problems 1, 2, & 3)
{-
eval env (Var x,s)     = (elookup x env,s)
eval env (Fun x e,s)   = (Closure (Fun x e) env, s)
eval env (App e1 e2,s) = eval (extend env' x val) (e1',s2)
    where (Closure (Fun x e1') env',s1) = eval env (e1,s)
          (val,s2)                      = eval env (e2,s1)

-}


-- Problem 1
-- Call-by-Value, dynamic scoping (Comment out for problems 2 and 3)

eval env (Var x,s)     = (elookup x env,s)
eval env (Fun x e,s)   = ((Fun x e),s)
eval env (App e1 e2,s) = eval (extend env x val) (e1'',s2)
    where ((Fun x e1''),s1) = eval env (e1,s)
          (val,s2)         = eval env (e2,s1)
{-
-}

-- Problem 2 
-- Call-by-Name, lexical scoping (Comment out for problems 1 and 3)
{-
eval env (Var x,s)  = eval env'' (e,s)
    where (Closure e env'',st) = (elookup x env,s)
eval env (Fun x e,s)   = (Closure (Fun x e) env, s)
eval env (App e1 e2,s) = eval (extend env'' x (Closure e2 env)) (e1'',s1)
    where (Closure (Fun x e1'') env'',s1) = eval env (e1,s)

-}

-- Problem 3
-- Call-by-Name, dynamic scoping (Comment out for problems 1 and 2)
{-
eval env (Var x,s)     = eval env (elookup x env,s)
eval env (Fun x e,s)   = (Fun x e,s)
eval env (App e1 e2,s) = eval (extend env x e2) (e1'',s1)
    where (Fun x e1'',s1) = eval env (e1,s)

-}

------------------------------------------------------------------------
-- rep e 
--   parses e, runs (eval emptyEnv (e,state4)), and prints the answer
rep :: String -> IO ()
rep e = do { putStrLn ("Evaluating:\n    " ++ e)
           ; let (e',s') = eval emptyEnv (eparse e,state4)
           ; putStr "Result:\n    "
           ; putStrLn (show e')
           ; putStr "    "
           ; prnState s'
           ; putStrLn ""
           }


------------------------------------------------------------------------
------------------------------------------------------------------------
-- Examples to test eval

-- to run, type: rep et0a
et0a = "{ print(0); print(1); print(2) }" 

-- to run, type: rep et0b
et0b = "{ X0 := 11; print(0); print(val(X0)); print(2); return val(X0) }"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- to run, type: rep et1
et1 = "let y = 1 in \
         \ let f = (fn x => (+ x y)) in \
              \ let y = 10 in (f 100)"
{- Under lexical scoping et1 should return 101
   Under dynamic scoping et1 should return 110
-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- to run, type: rep et2
et2 = "let f = (fn x => { X1 := (+ x val(X1)); return val(X1) })\
             \ in let x = (f 1) in (+ x (+ x x))"
{- Under call-by-value et2 should return 3 and have X[1]=1
   Under call-by-name  et2 should return 6 and have X[1]=3
-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- to run, type: rep et3
et3 = "let f = (fn x => { X1 := 100; return 0}) in \
             \ let g = (fn y => 12) in (g (f 9))"
{- Under call-by-value et3 should return 12 and have X[1]=100
   Under call-by-name  et3 should return 12 and have X[1]=0
-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- to run, type: rep et4
et4 = "let x = 1 in let f = (fn y => (* x y)) in \ 
             \ let g = (fn x => (f x)) in \
               \ print((g 10),(g 100),(g 1000))"
{- Under lexical scoping et4 should print
      PRINT: 10  100  1000
   Under dynamic scoping et1 should print
      PRINT: 100  10000  1000000
-}
                   
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- to run, type: rep et5
et5 = "let x = 1 in \
            \ let f = (fn n => { X1 := (+ val(X1) x); return n}) in \
               \ let x = 100 in \
                  \ let g = (fn z => (+ z z)) in \
                     \ (g (f 12))"

{- 
   Under call-by-value/lexical scoping et5 returns 24 with X[1]=1
   Under call-by-value/dynamic scoping et5 returns 24 with X[1]=100
   Under call-by-name/lexical scoping et5 returns 24 with X[1]=2
   Under call-by-name/dynamic scoping et5 returns 24 with X[1]=200
--}


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- to run, type: rep et6
et6 = "let f = (fn n => 0) in \
                \ let f = (fn n => if (< n 1) then 1 else (* n (f (- n 1)))) \
                 \ in (f 3)"
{- 
   Under lexical scoping et6 should return 0
   Under call-by-value dynamic scoping et6 should return 6
   Under call-by-name dynamic scoping et6 produces and infinite recursion.
-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
runAll     = mapM_ rep [et1, et2, et3, et4, et5, et6]
runAllbut6 = mapM_ rep [et1, et2, et3, et4, et5]
