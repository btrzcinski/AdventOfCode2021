module Lib
    ( minStraightLineFuelForAlignment,
    minArithmeticSumFuelForAlignment
    ) where


straightLine :: Int -> Int 
straightLine = id

arithmeticSum :: Int -> Int 
arithmeticSum 0 = 0
arithmeticSum u = (u * (1 + u)) `div` 2

minFuelForAlignment :: (Int -> Int) -> [Int] -> Int
minFuelForAlignment costFn s =
    let possiblePositions = [(minimum s)..(maximum s)]
        movementCosts = map (\p -> map (costFn . abs . (p -)) s) possiblePositions
    in minimum (map sum movementCosts)

minStraightLineFuelForAlignment :: [Int] -> Int
minStraightLineFuelForAlignment = minFuelForAlignment straightLine

minArithmeticSumFuelForAlignment :: [Int] -> Int 
minArithmeticSumFuelForAlignment = minFuelForAlignment arithmeticSum

