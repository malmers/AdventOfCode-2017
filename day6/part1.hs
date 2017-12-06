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

findPos :: [Int] -> Int -> Int -> Int
findPos (x:xs) elem pos | x == elem = pos
                        | otherwise = findPos xs elem (pos+1)

distribute :: [Int] -> Int -> Int -> [Int]
distribute list points pos | points == 0 = list
                           | otherwise = distribute (list !!= (pos, list !! pos + 1)) (points - 1) ((pos+1) `mod` (length list))

blockcycle :: [[Int]] -> Int -> Int
blockcycle blocks cycles | any (\x -> x == nextBlock) blocks = cycles
                         | otherwise = blockcycle (blocks ++ [nextBlock]) (cycles+1)
                where
                    block = last blocks
                    pos = findPos block (maximum block) 0
                    nextBlock = distribute (block !!= (pos, 0)) (maximum block) ((pos+1) `mod` length block)

main :: IO()
main = do
    input <- readFile "input"
    let contents = map strToInt (words input)
    let maxValue = maximum contents
    let maxIndex = findPos contents maxValue 0
    putStrLn $ show $ blockcycle [contents] 1
