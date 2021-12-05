module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rawSegments = lines input
    putStrLn $ "Overlapping points: " ++ show (numOverlappingPoints rawSegments)
