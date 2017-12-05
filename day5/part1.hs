module Main where
import Data.Char
import Data.List

strToInt :: String -> Int
strToInt s = read s

-- Update specific cell in a list
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) row (position, cell) | length b == 0 = row
                           | otherwise = a ++ [cell] ++ (drop 1 b)
    where
        ( a, b ) = splitAt position row

jumping :: [Int] -> Int -> Int
jumping list pos | jump + pos >= length list = 1
                 | otherwise = 1 + jumping (list !!= (pos, jump+1)) (pos + jump)
         where
            jump = list !! pos

main :: IO()
main = do
    input <- readFile "input"
    let contents = map strToInt (lines input)
    putStrLn $ show $ jumping contents 0
