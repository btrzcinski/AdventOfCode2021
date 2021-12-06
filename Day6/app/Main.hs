module Main where

import Data.List.Split (splitOn)
import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let school = map read $ splitOn "," input
    putStrLn $ "Number of fish after 80 days: " ++ show (numFishAfter school 80)
    putStrLn $ "Number of fish after 256 days: " ++ show (numFishAfter school 256)
