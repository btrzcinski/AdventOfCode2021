module Lib
    (
        pairs,
        triplets,
        increases,
        numIncreasingPairs,
        numIncreasingTripletSums
    ) where

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x, y):(pairs (y:xs))

triplets :: [Int] -> [(Int, Int, Int)]
triplets [] = []
triplets [x] = []
triplets [x,y] = []
triplets (x:y:z:xs) = (x,y,z):(triplets (y:z:xs))

increases :: (Int, Int) -> Bool
increases (x, y) = y > x

numIncreasingPairs :: [Int] -> Int
numIncreasingPairs x = length $ filter increases $ pairs x

sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

numIncreasingTripletSums :: [Int] -> Int
numIncreasingTripletSums x = length $ filter increases $ pairs $ map sumTriple $ triplets x
