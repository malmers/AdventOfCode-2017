module Main where
import Data.Char

strToInt :: String -> Int
strToInt s = read s

main :: IO()
main = do
    file <- readFile "input"
    let rows = map (\x -> map (strToInt) (words x)) (lines file)
    let rowSum = map (\x -> maximum x - minimum x) rows
    putStrLn $ show $ sum rowSum
