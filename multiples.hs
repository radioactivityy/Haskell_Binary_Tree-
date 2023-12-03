sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples ys n
  | 0 `elem` ys = error "Cannot have 0 in the list of multiples"
  | otherwise    = sum [x | x <- [1..n], any (\y -> x /= 0 && y /= 0 && x `mod` y == 0) ys]
