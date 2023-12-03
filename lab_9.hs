data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char deriving (Eq) 

 

instance (Show Expr) where 
    show = showExpr

eval :: Expr -> Int
eval (Num x) = x
eval (Add l r) = (eval l) + (eval r)
eval (Sub l r) = (eval l) - (eval r)
eval (Mul l r) = (eval l) * (eval r)
eval (Div l r) = (eval l) `div` (eval r)


showExpr :: Expr -> String
showExpr (Num x)  = show x
showExpr (Add l r) =  "(" ++ showExpr l ++ "+" ++ showExpr r ++ ")"
showExpr (Sub l r) =  "(" ++ showExpr l ++ "-" ++ showExpr r ++ ")"
showExpr (Mul l r) =  "(" ++ showExpr l ++ "*" ++ showExpr r ++ ")"
showExpr (Div l r) =  "(" ++ showExpr l ++ "/" ++ showExpr r ++ ")"
showExpr (Var x) = show x -- returning the value as a string

deriv :: Expr-> Char -> Expr


deriv (Num _) _ = Num 0
deriv (Var  x) v = Num 1

deriv (Add l r) v = Add (deriv l v) (deriv r v)  -- Sum rule
deriv (Sub l r) v = Sub (deriv l v) (deriv r v)  -- Difference rule
deriv (Mul l r) v = Add (Mul (deriv l v) r) (Mul l (deriv r v))  -- Product rule
deriv (Div l r) v = Div (Sub (Mul (deriv l v) r) (Mul l (deriv r v))) (Mul r r)  -- Quotient rule 
