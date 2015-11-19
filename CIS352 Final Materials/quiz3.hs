data Multi = Fork Int [Multi] deriving (Show,Eq)


autumn :: Multi -> Multi
autumn (Fork n xs) = (Fork n [autumn x | x <- xs, not (isLeaf x)] )


isLeaf :: Multi -> Bool
isLeaf (Fork _ []) = True
isLeaf (Fork _ _) = False