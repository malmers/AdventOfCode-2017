module Main where
import Data.Char

findLayer :: Int -> Int -> Int
findLayer i n | n <= (2*i+1)^2 = i
              | otherwise = findLayer (i+1) n

distance :: Int -> Int -> Int
distance layer n = layer + abs(middle - position)
    where min = (2*(layer-1)+1)^2
          max = (2*(layer)+1)^2
          modvalue = ((max - min) `quot` 4)
          middle = (modvalue `quot` 2) - 1
          position = (n-min-1) `mod` modvalue

main :: IO()
main = do
    let input = 325489
    let layer = findLayer 0 input
    let steps = distance layer input
    putStrLn $ show steps
