module Main where
import Data.Char
import Data.List

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = sort s1 == sort s2

isValid :: [String] -> Bool
isValid [] = True
isValid (x:xs) | any (\l -> isAnagram l x) xs = False
               | otherwise = isValid xs

main :: IO()
main = do
    input <- readFile "input"
    let contents = map isValid $ map (words) (lines input)
    let valid = length $ filter (\x -> x) contents
    putStrLn $ show valid
