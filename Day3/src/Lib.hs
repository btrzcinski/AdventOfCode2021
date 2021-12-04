{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( booleans,
    gammaRate,
    epsilonRate,
  )
where

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
