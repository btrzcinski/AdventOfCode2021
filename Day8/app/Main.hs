module Main where

import Lib (numUniqueSegmentOutputDigits, decodeOutputValue)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let numOutputOccurrences = foldr (\a b -> b + numUniqueSegmentOutputDigits a) 0 $ lines input
    putStrLn $ "Number of times 1,4,7,8 appear in output values: " ++ show numOutputOccurrences
    let sumOutputValues = sum $ map decodeOutputValue $ lines input
    putStrLn $ "Sum of output values: " ++ show sumOutputValues
