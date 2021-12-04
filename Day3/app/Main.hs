module Main where

import Lib (co2ScrubberRating, epsilonRate, gammaRate, lifeSupportRating, oxygenGeneratorRating, powerConsumption)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let binaryStrings = lines input
  putStrLn $ "Gamma rate: " ++ show (gammaRate binaryStrings)
  putStrLn $ "Epsilon rate: " ++ show (epsilonRate binaryStrings)
  putStrLn $ "Power consumption: " ++ show (powerConsumption binaryStrings)
  putStrLn $ "Oxygen generator rating: " ++ show (oxygenGeneratorRating binaryStrings)
  putStrLn $ "CO2 scrubber rating: " ++ show (co2ScrubberRating binaryStrings)
  putStrLn $ "Life support rating: " ++ show (lifeSupportRating binaryStrings)
