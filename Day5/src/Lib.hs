module Lib
    ( numOverlappingPoints,
        parseLineSegment,
        ventPointSet
    ) where

import Data.List.Split (splitOn)
import qualified Data.Tuple.Extra as T (both, second)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS (fromList, foldOccur)

numOverlappingPoints :: [String] -> Int 
numOverlappingPoints rawSegments =
    let s = ventPointSet rawSegments
    in MS.foldOccur (\_ o b -> if o > 1 then b + 1 else b) 0 s

ventPointSet :: [String] -> MultiSet (Int, Int)
ventPointSet rawSegments = MS.fromList $ concatMap parseLineSegment rawSegments

parseLineSegment :: String -> [(Int, Int)]
parseLineSegment s =
    let (start:end:_) = map (T.both read . T.second tail . break (== ',')) (splitOn " -> " s)
        lower = min start end
        upper = max start end
    in (if (fst start == fst end) || (snd start == snd end)
        then [(x,y) | x <- [(fst lower)..(fst upper)],
                      y <- [(snd lower)..(snd upper)]]
        else [])
