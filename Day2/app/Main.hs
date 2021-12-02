module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let commands = map parseLineToCommand $ lines input
    putStrLn $ "Final position score: " ++ (show $ uncurry (*) $ finalPosition commands)
    putStrLn $ "Final position score w/aim: " ++ (show $ uncurry (*) $ finalAimPosition commands)
