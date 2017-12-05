module Main where
import Data.Char
import Data.List
import qualified Data.Map as M

-- Compile this program with ghc before you run it

strToInt :: String -> Int
strToInt s = read s

unsafePos :: Maybe Int -> Int
unsafePos (Just n) = n

jumping :: M.Map Int Int -> Int -> Int -> Int -> Int
jumping list pos length steps | jump + pos >= length = steps + 1
                       | jump > 2 = jumping (M.insert pos (jump-1) list) (pos + jump) length (steps+1)
                       | otherwise = jumping (M.insert pos (jump+1) list) (pos + jump) length (steps+1)
         where
            jump = unsafePos $ M.lookup pos list

main :: IO()
main = do
    input <- readFile "input"
    let contents = map strToInt (lines input)
    let mymap = M.fromList $ zip [0..length contents] (contents)
    putStrLn $ show $ jumping mymap 0 (length contents) 0
