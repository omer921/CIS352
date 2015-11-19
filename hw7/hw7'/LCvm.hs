-- Vm2.hs 
--   Version 8 (Sun Mar  1 17:10:59 EST 2015)

module LCvm where

import Data.Word          (Word8)
import Data.Array.Unboxed (UArray,listArray,(!),(//))
import LC2                (Location,Wrd,boolToWrd)

fixMe fun = 
  error $ "fix me: " ++ fun

---------------------------------------------------------------------------
-- Memory banks
---------------------------------------------------------------------------
-- Bank = an array of 8-bit unsigned ints, indexed by 8-bit unsigned ints
-- bk!i = the bank bk's value at index i
type Bank = UArray Location Wrd

empty :: Bank
-- an empty (all 0's) memory bank
empty = listArray (minBound,maxBound) [0,0..]

set :: Bank -> Location -> Wrd -> Bank
-- (set b i c) = the result of setting b[i] to c.
set b i c = b // [(i,c)]

load :: Bank -> [Wrd] -> Bank
-- (load bnk vals) == the result of loading the vals sequentially into bnk
--   starting with location 0
load bnk vals = bnk // (zip [0..] vals)

---------------------------------------------------------------------------
-- The Op Codes 
---------------------------------------------------------------------------

data OpCode = HALT 
            | PUSH | POP  | DUP   | FETCH | STORE
            | IADD | ISUB | IMULT | ILT   | INC   | DEC
            | JMP  | JZ   | JNZ
            | CALL | RET
              deriving (Show,Eq,Enum,Bounded)

wrdToOp :: Wrd -> OpCode
-- translate from opcode number to an OpCode
wrdToOp = toEnum.fromEnum

opToWrd :: OpCode -> Wrd
-- translate from OpCode to an opcode number
opToWrd = toEnum.fromEnum

-- Assign names to the op-code numbers
[halt,push,pop,dup,fetch,store,iadd,isub,imult,ilt,inc,dec,jmp,jz,jnz,call,ret] 
    = map opToWrd [HALT,PUSH,POP,DUP,FETCH,STORE,
                   IADD,ISUB,IMULT,ILT,INC,DEC,JMP,JZ,JNZ,CALL,RET] 

---------------------------------------------------------------------------
-- Assembly code
---------------------------------------------------------------------------
data AsmCode = Halt 
             | Push Wrd | Pop | Dup | Fetch Wrd | Store Wrd
             | Iadd | Isub | Imult | Ilt | Inc | Dec
             | Jmp Wrd | Jz Wrd | Jnz Wrd
             | Call Wrd | Ret 
               deriving (Show)

assemble :: [AsmCode] -> [Wrd]
-- Translate a list of assembly instructions into machine code, e.g.,
--  assemble [Push 100,Push 20,Isub] = [1,100,1,20,6]
assemble instrs = concatMap atrans instrs


atrans :: AsmCode -> [Wrd]
-- Translate a single assembly instruction to machine code
atrans Halt      = [halt]
atrans (Push w)  = [push,w]
atrans Pop       = [pop]
atrans Dup       = [dup]
atrans (Fetch w) = [fetch,w]
atrans (Store w) = [store,w]
atrans Iadd      = [iadd]
atrans Isub      = [isub]
atrans Imult     = [imult]
atrans Ilt       = [ilt]
atrans Inc       = [inc]
atrans Dec       = [dec]
atrans (Jmp w)   = [jmp,w]
atrans (Jz w)    = [jz,w]
atrans (Jnz w)   = [jnz,w]
atrans (Call w)  = [call,w]
atrans Ret       = [ret]


disInstr :: Wrd -> Wrd -> AsmCode
-- (disInstr op arg) constructs the assembly instruction for op
-- (with the argument arg if needed)
disInstr op arg = case wrdToOp op of
                    HALT  -> Halt
                    PUSH  -> Push arg
                    POP   -> Pop
                    DUP   -> Dup
                    FETCH -> Fetch arg
                    STORE -> Store arg
                    IADD  -> Iadd
                    ISUB  -> Isub
                    IMULT -> Imult
                    ILT   -> Ilt
                    INC   -> Inc
                    DEC   -> Dec
                    JMP   -> Jmp arg
                    JZ    -> Jz arg
                    JNZ   -> Jnz arg
                    CALL  -> Call arg
                    RET   -> Ret


instrLen :: AsmCode -> Wrd
-- (instrLen instr) = the length of an assembly instruciton.
--   E.g., instrLen Pop = 1, instrLen (Push 8) = 2
instrLen instr = fromIntegral (length (atrans instr))


codeLen :: [AsmCode] -> Wrd
-- (codeLen is) = the length of a sequence of assemble instructions
--    when loaded into memory.
-- E.g. codeLen [Pop] = 1, codeLen [Push 8] = 2, codeLen [Pop,Push 8] = 3.
codeLen is = sum [instrLen instr | instr <- is]

prnAsm :: [AsmCode] -> IO ()
-- pretty prints an assembly program.  Try: (prnAsm fact4)
prnAsm instrs = mapM_ prnInstr (label 0 instrs)
    where
      label :: Wrd -> [AsmCode] -> [(Wrd,AsmCode)]
      label pc []      = []
      label pc (i:is)  =  (pc,i):label (pc + instrLen i) is

prnInstr :: (Wrd,AsmCode) -> IO ()
-- (prnInstr (n,instr)) prints assembly instruction instr together
-- with its location n
prnInstr (n,instr) = do 
    { putStr (formatWrd n ++ ": ")
    ; print instr
    } 

formatWrd :: Wrd -> String
-- I didn't feel like using printf. 
formatWrd n = take indent "   " ++ show n
  where
    indent = 3 - floor (log (fromIntegral n) / log 10)

---------------------------------------------------------------------------
-- The VM
---------------------------------------------------------------------------
-- a configuration consists of 
--   1. a program counter (ptr to the current instruction)
--   2. a stack pointer (1+the top of the stack)
--   3. a Bank containing the stack
--   4. a Bank containing the registers
-- The object code remains fixed in a run, so it is not part of 
-- the configuration.

type Config = (Wrd,Wrd,Bank,Bank)

blank :: Config 
blank = (0,0,empty,empty)

step :: Bank -> Config -> Config
-- One step of the VM, that is, 
--   (step obj c) = the next configuration of the machine
step obj (pc,sp,stk,regs) =
    let arg = obj!(pc+1)  -- the word following the current instr
        top = stk!(sp-1)  -- the top of the stack
        nxt = stk!(sp-2)  -- the elem of the stack just below the top
    
    in case wrdToOp (obj!pc) of 
        HALT  -> error "no next step!"
        PUSH  -> (pc+2, sp+1, set stk sp arg             , regs)
        POP   -> (pc+1, sp-1, stk                        , regs)
-- For Homework 7
        DUP   -> 
            if sp == 0
            then (pc+1, sp  , stk                         ,regs)
            else (pc+1, sp+1, set stk sp top             , regs)
        FETCH -> (pc+2, sp+1, set stk sp (regs!arg)      , regs)
        STORE -> (pc+2, sp  , stk                        , set regs arg top)
        IADD  -> (pc+1, sp-1, set stk (sp-2) (nxt+top)   , regs)
        ISUB  -> (pc+1, sp-1, set stk (sp-2) (nxt-top)   , regs)
        IMULT -> (pc+1, sp-1, set stk (sp-2) (nxt*top)   , regs)
        ILT   -> (pc+1, sp-1, set stk (sp-2) b           , regs)
            where b = boolToWrd (nxt<top)
-- For Homework 7
        INC   -> (pc+1, sp,   set stk (sp-1) (top + 1)   , regs)
-- For Homework 7
        DEC   -> (pc+1, sp,   set stk (sp-1) (top - 1)   , regs)
        JMP   -> (pc' , sp  , stk                        , regs)
          where 
            pc' = pc+arg+1

        JZ    -> (pc' , sp-1, stk                        , regs)
          where
            pc' = if top==0 then (pc+arg+1) else (pc+2)

        JNZ   -> (pc' , sp-1, stk                        , regs)
          where 
            pc' = if top/=0 then (pc+arg+1) else (pc+2)
-- For Homework 7
        CALL  -> (arg , sp+1, set stk sp (pc+2)          , regs)
-- For Homework 7
        RET   -> (back, sp-1, stk                        , regs)
          where
            back = stk!(sp-1)

halting :: Bank -> Config -> Bool
 -- (halting obj c) tests if c is a halting configuration
halting obj (pc,_,_,_) = (halt == obj!pc)

run  :: Bank -> Config -> Config
-- (run obj cfg) = run obj on cfg and return the final config
run  obj cfg = 
    if   halting obj cfg 
    then cfg   
    else run obj (step obj cfg)

run' :: Bank -> Config -> [Config]
-- (run' obj cfg) = run obj on cfg and return the course of configs
run' obj cfg = 
    if   halting obj cfg 
    then [cfg] 
    else cfg : run' obj (step obj cfg)

---------------------------------------------------------------------------
-- Utilities: printing VM runs
---------------------------------------------------------------------------

showRun :: [AsmCode] -> IO ()
-- showRun ap = assemble, load, and go (i.e., run the assembly program)
showRun ap = mapM_ (prn obj) trace   -- print the result
  where
    mc    = assemble ap    -- assem.~prog -> machine code
    obj   = load empty mc  -- load the machine code
    trace = run' obj blank -- a trace of the run

stepRun :: [AsmCode] -> IO ()
-- stepRun ap = assemble, load, and go (i.e., run the assembly program)
--                 and step through the run
stepRun ap = mapM_ showStep (zip [1..] trace)
  where
    mc    = assemble ap    -- assem.~prog -> machine code
    obj   = load empty mc  -- load the machine code
    trace = run' obj blank -- a trace of the run
    
    showStep (n,c ) = do   -- print a numbered config and read a char
      { putStr "Step "
      ; print n 
      ; prn obj c 
      ; getChar 
      }

stepRun' :: [AsmCode] -> IO ()
-- stepRun' ap = same as stepRun, but it prints the assembly program
--   before the run
stepRun' ap = do 
    { putStrLn "A run of:"
    ; prnAsm ap
    ; stepRun ap
    }

prn :: Bank -> Config -> IO ()
-- (prn obj config) prints the current instruction + configuration
prn obj (pc,sp,stk,regs) = do 
    { prnInstr (pc, disInstr (obj!pc) (obj!(pc+1)))
    ; putStr "Stack:     "
    ; print [stk!(j-1) | j <- [1..sp]]
    ; putStr "Regs(!=0): "
    ; mapM_ putStr registers
    ; putStrLn "\n"
    }
  where 
    reg i = 'x':show i
    registers = 
        [ reg i ++ "=" ++ show (regs!i) ++ " " 
        | i <- [minBound..maxBound]
        , regs!i /= 0
        ]

---------------------------------------------------------------------------
-- Run analysis tools
---------------------------------------------------------------------------

-- For Homework 7
--underOver :: [AsmCode] -> ([Int],[Int])
underOver asms = fixMe "error" 
uoTst1 = [Pop,Push 1,Pop,Push 1]


---------------------------------------------------------------------------
-- Examples
---------------------------------------------------------------------------

-- one way of computing 4!

fact4 = [Push 4, Store 0, Pop, Push 1, Store 1, Pop, Push 1, Fetch 0,
         Push 0, Isub, Push 1, Ilt, Isub, Jz 19, Fetch 1, Fetch 0, Imult,
         Store 1, Pop, Fetch 0, Push 1, Isub, Store 0, Pop, Jmp 226, Halt]

-- TEST FOR NEW INSTRUCTIONS

-- (incTest k) 
--   When run, (k+1) `mod` 256 should be at the top of the stack.
-- Try: (incTest 0) and (incTest 255)
incTest :: Wrd -> IO ()
incTest k = stepRun [Push k,Inc,Halt]

blankRun prog = prog `blankRunOn` blank

blankRunOn prog vm = 
     run (load empty (assemble prog)) vm

incWorks = (`all` [0.. 254]) $ \point ->
    let (_, _, stk, _) = blankRun [Push point, Inc, Halt]
    in  stk ! 0 == point + 1 

-- (decTest k) 
--   When run, (k-1) `mod` 256 should be at the top of the stack.
-- Try: (decTest 0) and (decTest 255)
decTest :: Wrd -> IO ()
decTest k = stepRun [Push k,Dec,Halt]

decWorks = flip all [1.. 255] $ \point ->
    let (_, _, stk, _) = blankRun [Push point, Dec, Halt]
    in  stk ! 0 == point - 1 

-- (dupTest ns)
--   When ns==[], the final stack should be empty
--   When ns==[n0,n1,...,nk], the final stack should be ns ++[nk]
dupTest :: [Wrd] -> IO ()
dupTest ns = stepRun ([Push n | n<-ns]++[Dup,Halt])

dupWorks = flip all [1.. 25] $ \point ->
    let (_, sp, stk, _) = blankRun ([Push n | n <- [1.. point]] ++ [Dup, Halt])
    in sp == point + 1
    && stk ! (sp - 1) == point

-- callRetTest k
--   when 0 <= k <= 245, this should run in 7 steps and halt
--      with x0=10 and 20 at the top of the stack.
--   when 246 <= k <= 255, various bad things happen (which is OK)
callRetTest :: Wrd -> IO ()
callRetTest k = stepRun ap
    where ap = [Call (k+5),Push 20,Halt]
                 ++(replicate (fromEnum k) Halt)
                 ++ [Push 10,Store 0,Pop,Ret]

-- another way of computing 4! using Dup, Call, and Ret.
--   This should take 67 steps and end with an empty stack and x1=24.
fact4' = [Push 4,  Store 0, Pop, Call 8, Halt,
          Fetch 0, Jnz 7, Push 1, Store 1, Pop, Ret,
          Fetch 0, Dup, Push 1, Isub, Store 0, Pop, Call 8, 
          Fetch 1, Imult, Store 1, Pop, Ret] 
