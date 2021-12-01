module Lib
    (
        pairs,
        increases,
        numIncreasingPairs
    ) where

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x, y):(pairs (y:xs))

increases :: (Int, Int) -> Bool
increases (x, y) = y > x

numIncreasingPairs :: [Int] -> Int
numIncreasingPairs x = length $ filter increases $ pairs x
