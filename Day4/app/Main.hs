module Main where

import Lib (firstWinningScore, lastWinningScore)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "First winning score: " ++ show (firstWinningScore input)
  putStrLn $ "Last winning score: " ++ show (lastWinningScore input)
