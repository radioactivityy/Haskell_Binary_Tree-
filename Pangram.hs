import Data.Char 

allLower :: String -> String
allLower = map toLower

alphabet :: String
alphabet = ['a'..'z']

isPangram :: String -> Bool
isPangram text = all ( `elem` allLower text ) alphabet
