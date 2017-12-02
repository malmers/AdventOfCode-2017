module Main where
import Data.Char

strToInt :: String -> Int
strToInt s = read s

divNumber :: [Int] -> Int
divNumber ns = head $ map (\(a, b) -> a `quot` b) $ filter (\(x, y) -> x `mod` y == 0 ) [ (x, y) | x <- ns, y <- ns, x /= y ]

main :: IO()
main = do
    file <- readFile "input"
    let rows = map (\x -> map (strToInt) (words x)) (lines file)
    let rowSum = map (\x -> maximum x - minimum x) rows
    putStrLn $ show $ sum $ map divNumber rows
