-- Omer Winrauke

import Control.Monad( liftM, liftM2 )
import Data.List( nub, sort )
import Test.QuickCheck( quickCheck, sample,
                        Arbitrary( arbitrary ), Gen,
                        oneof, elements, sized  )

same :: [Int] -> Bool 
same xs = and (zipWith (\x y -> x == y )xs (tail xs))


squash :: (a->a->b) -> [a] -> [b]
squash _ [] = []
squash f (x:z:xs) = (f x z): (squash f (z:xs))

squash' :: (a->a->b) -> [a] -> [b]
squash' f xs = (zipWith f xs (tail xs)) 

squash_pred xs = (squash (+) ys == squash' (+) ys)
       where ys = map (mod 1000) xs


data BTree = Empty | Branch Int BTree BTree
     deriving (Show)

foldT :: a -> (Int -> a -> a -> a) -> BTree -> a
foldT emp brch Empty = emp
foldT emp brch (Branch n tl tr) = brch n vl vr
      where vl = foldT emp brch tl
            vr = foldT emp brch tr


t1 = Branch 1 
       (Branch 2 
         (Branch 3 Empty Empty) 
         Empty) 
       (Branch 4 
         (Branch 5 Empty Empty) 
         (Branch 6 Empty Empty))



bsum t = foldT 0 f t
     where f n vl vr = n + vl + vr

bmaxDepth' t = foldT 0 g t
           where g n vl vr = 1 + max vl vr 