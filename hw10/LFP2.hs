module LFP2 where
-- last updated Wed Mar 11 16:16:28 EDT 2015
import State (Location)
import Test.QuickCheck
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map.Strict as M    

------------------------------------------------------------------------
-- Abstract syntax

data Expr = AConst Int | BConst Bool | Loc Location | Var String
          | ABin AOp Expr Expr | Compare COp Expr Expr 
          | Val Expr | Set Expr Expr | If Expr Expr Expr | Skip 
          | Seq Expr Expr | While Expr Expr
          | Fun String Expr | Let String Expr Expr | App Expr Expr 
          | Closure Expr Env | Print [Expr] | Return Expr
            deriving (Eq)
               
data AOp  = Plus | Minus | Times 
            deriving (Eq)

data COp  = Eq | Neq | Lt | Leq | Gt | Geq
            deriving (Eq)

------------------------------------------------------------------------
-- Environments and closures
type Name = String
type Env = M.Map Name Expr

emptyEnv :: Env
emptyEnv = M.empty

elookup :: Name -> Env -> Expr
elookup x env = case (M.lookup x env) of
                  (Just v) -> v
                  Nothing  -> error ("Unbound variable: "++x)

extend :: Env -> Name -> Expr -> Env
extend env x cls = M.insert x cls env

------------------------------------------------------------------------
-- Utilities

-- aApply iop n1 n2 = (n1 iop n2)
-- E.g., (aApply Plus 3 4) ~~> 7
aApply :: AOp -> Int -> Int -> Int
aApply Plus  n1 n2 = n1+n2
aApply Minus n1 n2 = n1-n2
aApply Times n1 n2 = n1*n2

-- bApply cop n1 n2 = (n1 cop n2)
-- E.g.:  (bApply Eq 3 4) ~~> False
-- E.g.:  (bApply Eq 3 3) ~~> True
bApply :: COp -> Int -> Int -> Bool
bApply Eq  v1 v2 = (v1==v2)
bApply Neq v1 v2 = (v1/=v2)
bApply Lt  v1 v2 = (v1<v2)
bApply Leq v1 v2 = (v1<=v2)
bApply Gt  v1 v2 = (v1>v2)
bApply Geq v1 v2 = (v1>=v2)

------------------------------------------------------------------------
-- Examples


------------------------------------------------------------------------
-- Display

instance Show Expr where
    show (AConst n)  = if n<0 then "("++show n++")" else show n
    show (Val e)     = "val(" ++ show e ++ ")"
    show (ABin iop e1 e2) 
        = "("++show iop ++ " " ++ show e1 ++" "++show e2++")"
    show (Compare cop e1 e2) 
        = "("++show cop ++ " " ++ show e1 ++" "++show e2++")"
    show (BConst b)  = if b then "tt" else "ff"
    show (Loc loc)   = 'X':show loc
    show Skip        = "skip"
    show (Set e1 e2) = show e1 ++ " := " ++ show e2
    show (Seq e1 e2) = "{ "++ show e1 ++ showRest e2 ++ " }"
        where showRest (Seq e1 e2) = "; "++show e1++showRest e2
              showRest e           = "; "++show e
    show (If e0 e1 e2) 
        = "if " ++ show e0 ++ " then " ++ show e1 ++ " else { "
          ++ show e2 ++ " }"
    show (While e0 e1) 
        = "while " ++ show e0 ++ " do " ++ show e1
    show (Var x) = x
    show (Fun x e) = "(fn "++x++"=>"++show e++")"
    show (App e0 e1) = "("++show op ++showArgs args++")"
        where 
          (op:args) = gather e0 [e1]
          gather (App e e') es = gather e (e':es)
          gather e es          = e:es
          showArgs es = concatMap (\e->' ':show e) es

    show (Let x e0 e1) = "let "++x++" = "++show e0++" in "++show e1
    show (Closure e env) = "[["++show e++"]]"
    show (Print es) = "print(" ++ intercalate "," (map show es) ++ ")"
    show (Return e) = "return "++show e
                     
instance Show AOp where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"

instance Show COp where
    show Eq = "=="
    show Neq = "/="
    show Lt  = "<"
    show Leq = "<="
    show Gt  = ">"
    show Geq = ">="



