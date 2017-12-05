module Main where
import Data.Char
import Data.List
import qualified Data.Map as M

-- Compile this program with ghc before you run it

jumping :: M.Map Int Int -> Int -> Int -> Int
jumping list pos steps | jump + pos >= M.size list = steps + 1
                       | jump > 2 = jumping (M.insert pos (jump-1) list) (pos + jump) (steps+1)
                       | otherwise = jumping (M.insert pos (jump+1) list) (pos + jump) (steps+1)
         where
            jump = list M.! pos

main :: IO()
main = do
    input <- readFile "input"
    let contents = map read (lines input)
    let mymap = M.fromList $ zip [0..length contents] (contents)
    putStrLn $ show $ jumping mymap 0 0
