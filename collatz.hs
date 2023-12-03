count :: Integer ->  Maybe Integer
count n = go n 0
 where
go :: Integer ->  Integer -> Maybe Integer
go n counter
        | n < 1 = Nothing -- absence of value
        | n == 1 = Just counter -- actual value
        | even n = go (n `div` 2) (counter +1 ) -- same thing as i++ in C
        | odd n = go (n*3 + 1) (counter +1 )

main :: IO ()
main = do
  let result = count 10  -- Replace 10 with the desired input value
  putStrLn $ "Result: " ++ show result