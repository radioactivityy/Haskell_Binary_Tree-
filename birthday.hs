import Data.Char 

birthday :: Int -> [Char]
birthday 1 = "First birthay"
birthday 18 = "You are an adult"
birthday 60 ="You are dead"
birthday age = "You are:   " ++ show age




responseFor :: String -> String
responseFor xs
  | all isSpace xs = "Fine. Be that way!"
  | last xs == '?' && all isUpper xs  = "Calm down, I know what I'm doing!" --it is not working, why
  | all isUpper xs = "Whoa, chill out!"
  | last xs == '?' = "Sure."
  | otherwise = "Whatever."


