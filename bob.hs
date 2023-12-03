module Bob (responseFor) where

import Data.Char (isUpper, isSpace)

responseFor :: String -> String

responseFor [] = "Fine. Be that way!"
responseFor xs
  | (last xs == '?') && all isUpper (init xs)  = "Calm down, I know what I'm doing!" 
  | last xs == '?' = "Sure."
  | all isUpper xs = "Whoa, chill out!"
  | otherwise = "Whatever."


