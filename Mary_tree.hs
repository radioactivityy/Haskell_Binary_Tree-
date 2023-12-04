import Language.Haskell.TH (Foreign(ImportF))
import Distribution.PackageDescription (PackageDescription(subLibraries))
data MTree a = MTree a [MTree a]


testTree1 :: MTree Int            
testTree1 = MTree 1 [(MTree 2 [(MTree 3 []),(MTree 4 [(MTree 5 []),(MTree 6 [])]), (MTree 7 []),(MTree 8 [])]), (MTree 9 [])]


msum :: MTree Int -> Int
msum (MTree a []) = a
msum (MTree a subtrees) = a + sum (map msum subtrees)

mToList :: MTree a -> [a]
mToList (MTree a []) = [a]
mToList (MTree a subtrees) = [a] ++ concatMap mToList subtrees

mLeafCount :: MTree a -> Int
mLeafCount (MTree _ []) = 1
mLeafCount (MTree _ subtrees) = sum (map mLeafCount subtrees) -- answer is 6

mMaxTree :: Ord a => MTree a -> a
mMaxTree (MTree a []) = a 
mMaxTree (MTree a subtrees) = maximum (map mMaxTree subtrees)

mContains :: Eq a => MTree a -> a -> Bool
mContains (MTree a []) n 
                | a == n = True
                | otherwise = False
mContains (MTree a subtrees) n 
                | a == n = True
                | otherwise = any (\subtree -> mContains subtree n) subtrees


mGreaterThan :: Ord a => MTree a -> a -> Int
mGreaterThan (MTree a []) n 
                    | a > n = 1
                    | otherwise = 0 
mGreaterThan(MTree a subtrees) n 
                    | a > n = 1 + sum (map (\subtree -> mGreaterThan subtree n) subtrees)
                    | otherwise = sum (map (\subtree -> mGreaterThan subtree n) subtrees)