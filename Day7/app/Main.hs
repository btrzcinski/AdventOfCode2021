module Main where

import Lib

import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let startingAlignments = map read $ splitOn "," input
    putStrLn $ "minStraightLineFuelForAlignment: " ++ show (minStraightLineFuelForAlignment startingAlignments)
