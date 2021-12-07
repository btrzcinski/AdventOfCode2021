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
    in foldr (\p c -> min c $ foldr (\x c' -> (c' +) $ costFn $ abs (p - x)) 0 s) maxBound possiblePositions


minStraightLineFuelForAlignment :: [Int] -> Int
minStraightLineFuelForAlignment = minFuelForAlignment straightLine

minArithmeticSumFuelForAlignment :: [Int] -> Int 
minArithmeticSumFuelForAlignment = minFuelForAlignment arithmeticSum

