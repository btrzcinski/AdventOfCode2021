{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( booleans,
    boolToCount,
    gammaRate,
    epsilonRate,
    powerConsumption,
    bitwiseFilter,
    bitwiseFilteredRating,
    oxygenGeneratorRating,
    co2ScrubberRating,
    lifeSupportRating,
  )
where

import Control.Applicative (liftA2)
import Data.List
import Util.Bits

booleans :: String -> [Bool]
booleans = map (== '1')

-- Endianness, for the fromListBE/fromListLE functions, refers to
-- BIT ORDER, not just byte order. (So you almost always want
-- big endian in this situation.)
bitwiseRate :: ([Bool] -> Bool) -> [String] -> Int
bitwiseRate f x = fromListBE $ map f $ transpose $ map booleans x

boolToCount :: Bool -> Int
boolToCount False = -1
boolToCount True = 1

gammaRate :: [String] -> Int
gammaRate = bitwiseRate (\x -> sum (map boolToCount x) > 0)

epsilonRate :: [String] -> Int
epsilonRate = bitwiseRate (\x -> sum (map boolToCount x) < 0)

powerConsumption :: [String] -> Int
powerConsumption = liftA2 (*) gammaRate epsilonRate

bitwiseFilter :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
bitwiseFilter _ [] = []
bitwiseFilter _ [x] = x
bitwiseFilter f lst =
  let filterBit = f (head $ transpose lst)
   in filterBit : bitwiseFilter f (map tail $ filter (\x -> head x == filterBit) lst)

bitwiseFilteredRating :: ([Bool] -> Bool) -> [String] -> Int
bitwiseFilteredRating f lst = fromListBE $ bitwiseFilter f (map booleans lst)

oxygenGeneratorRating :: [String] -> Int
oxygenGeneratorRating = bitwiseFilteredRating (\x -> sum (map boolToCount x) >= 0)

co2ScrubberRating :: [String] -> Int
co2ScrubberRating = bitwiseFilteredRating (\x -> sum (map boolToCount x) < 0)

lifeSupportRating :: [String] -> Int
lifeSupportRating = liftA2 (*) oxygenGeneratorRating co2ScrubberRating