module Main where

import Lib (totalRiskLevel)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let r = totalRiskLevel $ lines input
    putStrLn $ "Total risk level: " ++ show r
