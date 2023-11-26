import Stack  
import Queue

data Tree a = Leaf a 
            | Branch a (Tree a) (Tree a) deriving (Show)

tree1 :: Tree Int
tree1 = Branch 1 (Leaf 2) (Branch 4 (Leaf 5) (Leaf 6))


sum' :: Tree Int -> Int 
sum' (Leaf x) = x 
sum' (Branch x l r ) = sum' l + x + sum' r 


toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList  (Branch x l r ) = toList l ++ [x] ++ toList r 


maxTree :: Ord a => Tree a -> a 
maxTree (Leaf x) = x 
maxTree (Branch x l r)  =  maximum [x, maxTree l, maxTree r]

depthTree :: Tree a -> Int
depthTree (Leaf x) = 1
depthTree (Branch x l r) = 1 + max (depthTree l) (depthTree r)


getGreaterElements :: Ord a => Tree a -> a -> [a]
getGreaterElements (Leaf x) y 
                            | x > y  = [x] 
                            | otherwise = [y] 

getGreaterElements (Branch x l r) y 
                            | x > y = [x] ++ getGreaterElements l y ++ getGreaterElements r y   
                            | otherwise = getGreaterElements l y ++ getGreaterElements r y

contains :: Eq a => Tree a -> a -> Bool 
contains (Leaf x) y = x == y --comparing
contains (Branch x l r) y = x == y || contains l y || contains r y -- or 



toString :: Show a => Tree a -> String
toString (Leaf x) = show x 
toString (Branch x l r) = show x ++ "(" ++ toString l ++"," ++ toString r ++  " )"



leafCount :: Tree a -> Int
leafCount (Leaf x) = 1
leafCount (Branch x l r ) = (leafCount l) + (leafCount r)
                    


branchCount :: Tree a -> Int
branchCount (Leaf x) = 0
branchCount (Branch x l r ) = (branchCount l) + 1 + (branchCount r)

greaterThan :: Ord a => Tree a -> a -> Int
greaterThan (Leaf x) y 
                   | x > y = 1 
                   | otherwise = 0 
greaterThan (Branch x l r) y 
                    |x > y = 1
                    | otherwise = greaterThan l y  + greaterThan r y  


