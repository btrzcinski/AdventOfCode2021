{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( numFishAfter
    ) where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

class School s where
    numFishAfter :: s -> Int -> Int

instance School [Int] where
    numFishAfter s = numFishAfter (MS.fromList s)

instance School (MultiSet Int) where
    numFishAfter s 0 = MS.foldOccur (\_ o b -> o + b) 0 s
    numFishAfter s d =
        numFishAfter (MS.concatMap nextFishCycle s) (d - 1)

nextFishCycle :: Int -> [Int]
nextFishCycle c
    | c == 0 = [6,8]
    | otherwise = [c - 1]
