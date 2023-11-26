module Stack where 
 
data Stack a = St [a] deriving (Show)

push :: a -> Stack a -> Stack a
push x (St list) = St (x:list)

pop :: Stack a -> Stack a
pop (St(x:xs)) = St xs

top :: Stack a -> a
top (St(x:xs)) =  x

isEmpty :: Stack a ->Bool
isEmpty (St xs) = null xs