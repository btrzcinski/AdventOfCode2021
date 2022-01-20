import Lib
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

main :: IO ()
main = defaultMain unitTests

testData :: [String]
testData = [
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
    ]

unitTests :: TestTree
unitTests = testGroup
    "Unit tests" [
        testCase "totalRiskLevel" $
            totalRiskLevel testData @?= 15
    ]