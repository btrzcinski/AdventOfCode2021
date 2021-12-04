import Test.Tasty
import Test.Tasty.HUnit
import Data.Matrix (fromLists)
import Lib

main :: IO ()
main = do
    testData <- readFile "testData.txt"
    defaultMain $ unitTests testData

unitTests :: String -> TestTree
unitTests testData =
  testGroup
    "Unit tests"
    [ testCase "firstWinningScore (Part 1)" $
        firstWinningScore testData @?= 4512,
      testCase "drawSequence" $
        drawSequence (head $ lines testData) @?= [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
      testCase "parseBoards" $
        parseBoards (tail $ lines testData) @?=
            [
                fromLists ([
                    [22,13,17,11,0],
                    [8,2,23,4,24],
                    [21,9,14,16,7],
                    [6,10,3,18,5],
                    [1,12,20,15,19]
                    ] :: [[Int]]),
                fromLists ([
                    [3,15,0,2,22],
                    [9,18,13,17,5],
                    [19,8,7,25,23],
                    [20,11,10,24,4],
                    [14,21,16,12,6]
                    ] :: [[Int]]),
                fromLists ([
                    [14,21,17,24,4],
                    [10,16,15,9,19],
                    [18,8,23,26,20],
                    [22,11,13,6,5],
                    [2,0,12,3,7]
                    ] :: [[Int]])
            ],
        testCase "score" $
            score (fromLists ([
                    [14,21,17,24,4],
                    [10,16,15,9,19],
                    [18,8,23,26,20],
                    [22,11,13,6,5],
                    [2,0,12,3,7]
                    ] :: [[Int]]))
            [7,4,9,5,11,17,23,2,0,14,21,24] @?= 4512,
        testCase "isWinningBoard" $
            isWinningBoard (fromLists ([
                    [14,21,17,24,4],
                    [10,16,15,9,19],
                    [18,8,23,26,20],
                    [22,11,13,6,5],
                    [2,0,12,3,7]
                    ] :: [[Int]]))
            [7,4,9,5,11,17,23,2,0,14,21,24] @? "wins on first row",
        testCase "firstWinningCondition" $
            firstWinningCondition [
                fromLists ([
                    [22,13,17,11,0],
                    [8,2,23,4,24],
                    [21,9,14,16,7],
                    [6,10,3,18,5],
                    [1,12,20,15,19]
                    ] :: [[Int]]),
                fromLists ([
                    [3,15,0,2,22],
                    [9,18,13,17,5],
                    [19,8,7,25,23],
                    [20,11,10,24,4],
                    [14,21,16,12,6]
                    ] :: [[Int]]),
                fromLists ([
                    [14,21,17,24,4],
                    [10,16,15,9,19],
                    [18,8,23,26,20],
                    [22,11,13,6,5],
                    [2,0,12,3,7]
                    ] :: [[Int]])
            ] [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1] @?= (fromLists ([
                    [14,21,17,24,4],
                    [10,16,15,9,19],
                    [18,8,23,26,20],
                    [22,11,13,6,5],
                    [2,0,12,3,7]
                    ] :: [[Int]]), [7,4,9,5,11,17,23,2,0,14,21,24]),
        testCase "lastWinningScore (Part 2)" $
            lastWinningScore testData @?= 1924,
        testCase "lastWinningCondition" $
            lastWinningCondition [
                fromLists ([
                    [22,13,17,11,0],
                    [8,2,23,4,24],
                    [21,9,14,16,7],
                    [6,10,3,18,5],
                    [1,12,20,15,19]
                    ] :: [[Int]]),
                fromLists ([
                    [3,15,0,2,22],
                    [9,18,13,17,5],
                    [19,8,7,25,23],
                    [20,11,10,24,4],
                    [14,21,16,12,6]
                    ] :: [[Int]]),
                fromLists ([
                    [14,21,17,24,4],
                    [10,16,15,9,19],
                    [18,8,23,26,20],
                    [22,11,13,6,5],
                    [2,0,12,3,7]
                    ] :: [[Int]])
            ] [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1] @?= (fromLists ([
                [3,15,0,2,22],
                [9,18,13,17,5],
                [19,8,7,25,23],
                [20,11,10,24,4],
                [14,21,16,12,6]
            ] :: [[Int]]), [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13])
    ]