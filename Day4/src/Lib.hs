module Lib
    ( firstWinningScore,
    drawSequence,
    parseBoards,
    score,
    isWinningBoard,
    firstWinningCondition,
    lastWinningCondition,
    lastWinningScore
    ) where

import Data.List (inits)
import Data.List.Split (splitOn, chunksOf)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M (fromList, toList, toLists, transpose)
import Data.Char (isNumber)
import qualified Data.Set as S (difference, fromList, member)
import Data.Bifunctor (Bifunctor(first))

boardSize :: Int
boardSize = 5

lastWinningScore :: String -> Int
lastWinningScore = winningScore lastWinningCondition

firstWinningScore :: String -> Int
firstWinningScore = winningScore firstWinningCondition

winningScore :: ([Matrix Int] -> [Int] -> (Matrix Int, [Int])) -> String -> Int
winningScore conditionFunc input =
    let (rawDrawSeq:rawBoards) = lines input
        drawSeq = drawSequence rawDrawSeq
        boards = parseBoards rawBoards
        (winningBoard, winningSequence) = conditionFunc boards drawSeq
    in score winningBoard winningSequence

lastWinningCondition :: [Matrix Int] -> [Int] -> (Matrix Int, [Int])
lastWinningCondition boards drawList =
    let drawSequences = tail $ inits drawList
        checkSequence' = flip isWinningBoard
        outcomes = map (\seq -> filter (checkSequence' seq) boards) drawSequences
        reversedConditions = reverse $ zip outcomes drawSequences
        maxWinningBoards = length $ fst $ head reversedConditions
        (lastConditions, otherConditions) = span (\x -> length (fst x) == maxWinningBoards) reversedConditions
        ineligibleBoards = fst $ head otherConditions
    in first (head . filter (`notElem` ineligibleBoards)) $ last lastConditions

firstWinningCondition :: [Matrix Int] -> [Int] -> (Matrix Int, [Int])
firstWinningCondition boards drawList =
    let drawSequences = tail $ inits drawList
        checkSequence' = flip isWinningBoard
        outcomes = map (\seq -> filter (checkSequence' seq) boards) drawSequences
        conditions = zip outcomes drawSequences
    in first head $ head $ filter (not . null . fst) conditions

isWinningBoard :: Matrix Int -> [Int] -> Bool
isWinningBoard _ [] = False
isWinningBoard board markedNums =
    let markedSet = S.fromList markedNums
        rows = M.toLists board
        columns = M.toLists $ M.transpose board
        possibleCombinations = rows ++ columns
        isMarked' = flip S.member markedSet
    in any (all isMarked') possibleCombinations

score :: Matrix Int -> [Int] -> Int
score _ [] = 0
score board markedNums =
    let unmarkedSum = sum (S.difference (S.fromList (M.toList board)) (S.fromList markedNums))
    in unmarkedSum * last markedNums

drawSequence :: String -> [Int]
drawSequence x = map read $ splitOn "," x

parseBoards :: [String] -> [Matrix Int]
parseBoards x = splitBoards $ map read $ filter (any isNumber) $ concatMap (splitOn " ") x

splitBoards :: [Int] -> [Matrix Int]
splitBoards [] = []
splitBoards x = map (M.fromList boardSize boardSize) (chunksOf (boardSize * boardSize) x)