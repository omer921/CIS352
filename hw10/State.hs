module State where
import qualified Data.Map.Strict as M

------------------------------------------------------------------------
-- modeling state

type Location = Int 
type State = M.Map Location Int

-- empty = an empty state
empty :: State
empty = M.empty

-- fetch s i = s(i), if i is in dom(s), o/w BOOM!
fetch :: State -> Location -> Int
fetch s i = case (M.lookup i s) of
              Just v  -> v
              Nothing -> error ("location " ++ show i ++ " not in the state")
-- fetch s i = M.findWithDefault 0 i s  -- Alt: o/w 0

-- set s i v = s[i |-> v]
set :: State -> Location -> Int -> State
set s i v = M.insert i v s

-- load s [x0,x1,...,xk] = the modification of s with s(i)=xi, i=0,...,k.
load :: State -> [Int] -> State
load s vs = M.fromDistinctAscList $ zip [0..] vs

-- dump s = [(i,s(i)) : i is in dom(s)]
dump :: State -> [(Location,Int)]
dump s = M.toList s

------------------------------------------------------------------------
prnState :: State -> IO ()
prnState s 
    = do { let ps = [ (i,v) | (i,v) <- dump s, v /= 0]
         ; putStr "Locs(!=0): "
         ; mapM_ putStr ["X["++show i++"]="++show (x)++" " | (i,x) <- ps]
         ; putStrLn ""
         }

------------------------------------------------------------------------

-- a state with state4(0)=4 state4(1)=...=state4(4)=0.
state4 = load empty [4,0,0,0,0]
--        locations: 0 1 2 3 4
