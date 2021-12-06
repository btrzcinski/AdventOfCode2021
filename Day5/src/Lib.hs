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

-- Line segment lower/upper points must be ordered left -> right, top -> bottom.
walkLineSegment :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
walkLineSegment lower upper
    | (fst lower == fst upper) || (snd lower == snd upper) =
         [(x,y) | x <- [(fst lower)..(fst upper)],
                  y <- [(snd lower)..(snd upper)]]
    | snd lower < snd upper = zip [(fst lower)..(fst upper)] [(snd lower)..(snd upper)]
    | otherwise = zip [(fst lower)..(fst upper)] [(snd lower),(pred $ snd lower)..(snd upper)]


parseLineSegment :: String -> [(Int, Int)]
parseLineSegment s =
    let (start:end:_) = map (T.both read . T.second tail . break (== ',')) (splitOn " -> " s)
        lower = min start end
        upper = max start end
    in walkLineSegment lower upper
