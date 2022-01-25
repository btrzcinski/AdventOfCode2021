module Lib
  ( totalRiskLevel,
  )
where

import qualified Data.Matrix as M (fromLists, mapPos, safeGet, toList)
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)

totalRiskLevel :: [String] -> Int
totalRiskLevel rows =
  sum $ map ((+ 1) . point) $ filter (\al -> all (\n -> n > point al) (neighbors al)) (toAdjacencyLists rows)

data AdjacencyList = AdjacencyList {point :: Int, neighbors :: [Int]}

toAdjacencyLists :: [String] -> [AdjacencyList]
toAdjacencyLists rows =
  let mx = (M.mapPos (\_ x -> digitToInt x) $ M.fromLists rows)
   in M.toList $ M.mapPos (\(r, c) x -> AdjacencyList x (mapMaybe (\(r', c') -> M.safeGet r' c' mx) [(r -1, c), (r + 1, c), (r, c -1), (r, c + 1)])) mx
