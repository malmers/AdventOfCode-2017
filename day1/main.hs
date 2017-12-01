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
    let half = splitAt (length contents `div` 2) contents
    let contents2 = zip (fst half) (snd half)
    let contents3 = sum $ map pairvalue contents2
    putStrLn $ show $ contents3 * 2
