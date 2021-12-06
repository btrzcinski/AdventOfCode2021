import Lib
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.MultiSet as MS (fromList)

main :: IO ()
main = defaultMain unitTests

testData :: [String]
testData = [
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
    ]

unitTests :: TestTree
unitTests = testGroup
    "Unit tests" [
        testCase "numOverlappingPoints (Part 2 sample)" $
            numOverlappingPoints testData @?= 12,
        testCase "parseLineSegment for horizontal segments" $
            parseLineSegment "0,9 -> 5,9" @?= [(0,9),(1,9),(2,9),(3,9),(4,9),(5,9)],
        testCase "parseLineSegment for vertical segments" $
            parseLineSegment "7,0 -> 7,4" @?= [(7,0),(7,1),(7,2),(7,3),(7,4)],
        testCase "parseLineSegment for backwards horizontal segments" $
            parseLineSegment "5,9 -> 0,9" @?= [(0,9),(1,9),(2,9),(3,9),(4,9),(5,9)],
        testCase "parseLineSegment for backwards vertical segments" $
            parseLineSegment "7,4 -> 7,0" @?= [(7,0),(7,1),(7,2),(7,3),(7,4)],
        testCase "parseLineSegment for LL -> UR diagonals" $
            parseLineSegment "7,9 -> 9,7" @?= [(7,9),(8,8),(9,7)],
        testCase "parseLineSegment for UR -> LL diagonals" $
            parseLineSegment "9,7 -> 7,9" @?= [(7,9),(8,8),(9,7)],
        testCase "parseLineSegment for LR -> UL diagonals" $
            parseLineSegment "9,9 -> 7,7" @?= [(7,7),(8,8),(9,9)],
        testCase "parseLineSegment for UL -> LR diagonals" $
            parseLineSegment "7,7 -> 9,9" @?= [(7,7),(8,8),(9,9)]
    ]
