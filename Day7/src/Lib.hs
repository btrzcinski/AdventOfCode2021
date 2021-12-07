module Lib
    ( minStraightLineFuelForAlignment,
    minArithmeticSumFuelForAlignment
    ) where

minStraightLineFuelForAlignment :: [Int] -> Int
minStraightLineFuelForAlignment s =
    let possiblePositions = [(minimum s)..(maximum s)]
        movementCosts = map (\p -> map (abs . (p -)) s) possiblePositions
    in minimum (map sum movementCosts)

arithmeticSum :: Int -> Int 
arithmeticSum 0 = 0
arithmeticSum u = (u * (1 + u)) `div` 2

minArithmeticSumFuelForAlignment :: [Int] -> Int
minArithmeticSumFuelForAlignment s =
    let possiblePositions = [(minimum s)..(maximum s)]
        movementCosts = map (\p -> map (arithmeticSum . abs . (p -)) s) possiblePositions
    in minimum (map sum movementCosts)
