data Tree a = Leaf a 
           | Branch a (Tree a) (Tree a ) deriving (Show)



testTree2 :: Tree Char
testTree2 = Branch 'a' (Branch 'c' (Leaf 'z') (Leaf 'z')) (Leaf 'v')

-- data Tree3 a = Null -- for an empty tree
--             | Branch a (Tree3 a) (Tree3 a) deriving (Show) -- for non empty tree

sum' :: Tree Int -> Int
sum' (Leaf x) = x
sum' (Branch x l r) = sum' l + x +  sum' r

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch x l r) = toList l++ [x] ++ toList r 

maxTree :: Ord a => Tree a -> a
maxTree (Leaf x) = x
maxTree (Branch x l r ) = maximum  [x, maxTree l, maxTree r] -- creating list, and taking max from the list

depthTree :: Tree a -> Int
depthTree (Leaf x) = 1 
depthTree (Branch x l r ) =  depthTree (l) + depthTree (r) 

getGreaterElements :: Ord a => Tree a -> a -> [a]
getGreaterElements (Leaf x) n 
                    | x >  n = [x]
                    | otherwise = [] 
getGreaterElements (Branch x l r) n 
                    | x > n = x:getGreaterElements l n ++ getGreaterElements r n
                    | otherwise = getGreaterElements l n ++ getGreaterElements r n 
                    


leafCount :: Tree a -> Int
leafCount (Leaf x) = 1
leafCount (Branch x l r) =  leafCount l + leafCount r

branchCount :: Tree a -> Int
branchCount (Leaf x) = 0
branchCount (Branch x l r) =  1 + branchCount l + branchCount r 

contains :: Eq a => Tree a -> a -> Bool
contains  (Leaf x) n 
                    | x ==  n = True
                    | otherwise = False
contains (Branch x l r) n 
                    | x == n = True
                    | otherwise = False


greaterThan :: Ord a => Tree a -> a -> Int
greaterThan (Leaf x) n 
                    | x >  n = 1 
                    | otherwise = 0 
greaterThan (Branch x l r) n 
                    | x > n = 1 +  greaterThan l n + greaterThan r n
                    | otherwise = greaterThan l n + greaterThan r n