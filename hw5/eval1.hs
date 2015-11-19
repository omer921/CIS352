-- Omer Winrauke

import AS1
import Parser1
import Debug.Trace

fixMe = error "Please fix me"
throwCode = error "Can't divide these integers"
-- eval e | trace ("entering eval with arg: "++ show e) False = undefined 
eval (Num n)          = n
eval (Add a1 a2)      = (eval a1) + (eval a2)
eval (Sub a1 a2)      = (eval a1) - (eval a2)
eval (Mult a1 a2)     = (eval a1) * (eval a2)
eval (Div a1 a2)      = if ((eval a1) > (eval a2)) then (if ((eval a2) == 0) then 0 else (div (eval a1) (eval a2))) else throwCode
eval (Cond a1 a2 a3 ) = if ((eval a1) == 0) then (eval a3) else (eval a2)

------------------------------------------------------------------------
-- run e 
--   parses e, evaluates e, prints the answer
--   Try: (run "2+3*5")
run :: String -> IO ()
run etxt = do { let e = aparse etxt
              ; putStrLn $ "Evaluating: " ++ show e
              ; let val = eval e
              ; putStrLn $ "    Result: " ++ show val
              }

-- read-eval-print
rep :: IO ()
rep = do { etxt <- getLine;
         ; let e = aparse etxt
         ; let val = eval e
         ; putStrLn $ "Evaluates to:\t"++ show val
         }
           