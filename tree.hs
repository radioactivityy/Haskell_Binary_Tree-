import Data.Binary.Get (label, isEmpty)
data Tree a = Null 
            | Branch a (Tree a) (Tree a) 
            deriving (Show)

root (Branch x _ r) = x
left (Branch _ l _) = l

right (Branch _ _ r) = r

isEmpty Null = True
isEmpty _ = False 

size Null = 0
size (Branch _ l r) = 1 + (size l) + (size r)

preOrder Null = []
preOrder (Branch x l r) = x: ((preOrder l) ++ (preOrder r))

