module Lib
    ( numUniqueSegmentOutputDigits,
    decodeOutputValue,
    deriveDigitMap
    ) where

import Data.List (sort, sortOn)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.Tuple.Extra as TupleExtra (first)

numUniqueSegmentOutputDigits :: String -> Int
numUniqueSegmentOutputDigits line =
    let outputDigits = splitOn " " $ last (splitOn " | " line)
        digitHasUniqueSegments' d = length d `elem` [2,3,4,7]
    in foldr (\a b -> if digitHasUniqueSegments' a then b + 1 else b) 0 outputDigits

-- Decoding input segments:
-- * Knowing the values of 1,4,7,8 from unique segment lengths
-- * Take the frequencies of each segment in the input set
-- * e occurs 4x
-- * f occurs 9x
-- * b occurs 6x
-- * d is the difference of 4 - 1 - b (and occurs 7x)
-- * g is the other one that occurs 7x
-- * a is the difference of 7 - 1 (and occurs 8x)
-- * c is the other one that occurs 8x


freqDigit :: Int -> [Set Char] -> [Char]
freqDigit f s =
    let occurList = zip "abcdefg" $ map (\d -> foldr ((\a b -> if a then b + 1 else b) . Set.member d) 0 s) "abcdefg"
    in  map fst $ filter (\(d, o) -> o == f) occurList

-- produces a map from [sorted segment letters] -> number
deriveDigitMap :: String -> Map String Char
deriveDigitMap line =
    let inputDigits = splitOn " " $ head (splitOn " | " line)
        digitSets = map Set.fromList inputDigits
        (one:seven:four:_) = sortOn Set.size digitSets
        e = head (freqDigit 4 digitSets)
        f = head (freqDigit 9 digitSets)
        b = head (freqDigit 6 digitSets)
        d = Set.elemAt 0 $ Set.delete b $ Set.difference four one
        g = head $ filter (/= d) (freqDigit 7 digitSets)
        a = Set.elemAt 0 $ Set.difference seven one
        c = head $ filter (/= a) (freqDigit 8 digitSets)
    in Map.fromList (map (TupleExtra.first sort) [([a,b,c,e,f,g], '0'),
                     ([c,f], '1'),
                     ([a,c,d,e,g], '2'),
                     ([a,c,d,f,g], '3'),
                     ([b,c,d,f], '4'),
                     ([a,b,d,f,g], '5'),
                     ([a,b,d,e,f,g], '6'),
                     ([a,c,f], '7'),
                     ([a,b,c,d,e,f,g], '8'),
                     ([a,b,c,d,f,g], '9')])


decodeOutputValue :: String -> Int
decodeOutputValue line =
    let digitMap = deriveDigitMap line
        scrambledOutputDigits = map sort $ splitOn " " $ last (splitOn " | " line)
    in read $ map (digitMap Map.!) scrambledOutputDigits
