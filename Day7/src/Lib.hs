module Lib
    ( minStraightLineFuelForAlignment
    ) where

minStraightLineFuelForAlignment :: [Int] -> Int
minStraightLineFuelForAlignment s =
    let possiblePositions = [(minimum s)..(maximum s)]
        movementCosts = map (\p -> map (abs . (p -)) s) possiblePositions
    in minimum (map sum movementCosts)

