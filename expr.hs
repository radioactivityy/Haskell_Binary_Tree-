data Expr = Num Integer  
         | Add Expr Expr 
         | Sub Expr Expr deriving (Show)

eval :: Expr -> Integer 
eval (Num x) = x
eval (Add l e2) = (eval l) + (eval e2)
eval (Sub l e2) = (eval l) - (eval e2)  