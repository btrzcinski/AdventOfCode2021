module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let nums = map read $ lines input
    putStrLn $ "Num increasing pairs: " ++ (show $ numIncreasingPairs nums)
    putStrLn $ "Num increasing triplet sums: " ++ (show $ numIncreasingTripletSums nums)
