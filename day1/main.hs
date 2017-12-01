module Main where
import Data.Char

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

pairvalue :: (Int, Int) -> Int
pairvalue (a, b) | a == b = a
                 | otherwise = 0

main :: IO()
main = do
    file <- readFile "input"
    let contents = map digitToInt file
    let contents2 = contents ++ [head contents]
    let contents3 = sum $ map pairvalue (pairs contents2)
    putStrLn $ show contents3
