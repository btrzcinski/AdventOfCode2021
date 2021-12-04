module Main where

import Lib (epsilonRate, gammaRate)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let binaryStrings = lines input
  let g = gammaRate binaryStrings
  let e = epsilonRate binaryStrings
  putStrLn $ "Gamma rate: " ++ show g
  putStrLn $ "Epsilon rate: " ++ show e
  putStrLn $ "g * e = " ++ show (g * e)
