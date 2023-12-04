data Point = Point Float Float deriving (Show, Eq)

pointExample = Point 2.0 3.0

pointExample1:: Point -> Bool 
pointExample1 (Point x y) 
                | x == y = True
                | otherwise = False 