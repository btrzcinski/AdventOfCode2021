module Main where

import Lib (firstWinningScore)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "First winning score: " ++ show (firstWinningScore input)