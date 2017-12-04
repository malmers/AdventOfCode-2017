module Main where
import Data.Char

isValid :: [String] -> Bool
isValid [] = True
isValid (x:xs) | any (\l -> l == x) xs = False
               | otherwise = isValid xs

main :: IO()
main = do
    input <- readFile "input"
    let contents = map isValid $ map (words) (lines input)
    let valid = length $ filter (\x -> x) contents
    putStrLn $ show valid
