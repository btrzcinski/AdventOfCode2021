module Lib
    ( numFishAfter
    ) where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

numFishAfter :: [Int] -> Int -> Int
numFishAfter s = numFishAfterInSet (MS.fromList s)

numFishAfterInSet :: MultiSet Int -> Int -> Int 
numFishAfterInSet s 0 = MS.foldOccur (\_ o b -> o + b) 0 s
numFishAfterInSet s d =
    numFishAfterInSet (MS.concatMap nextFishCycle s) (d - 1)

nextFishCycle :: Int -> [Int]
nextFishCycle c
    | c == 0 = [6,8]
    | otherwise = [c - 1]
