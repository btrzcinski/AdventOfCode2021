import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Day1-1 example" $
        numIncreasingPairs [
            199,
            200,
            208,
            210,
            200,
            207,
            240,
            269,
            260,
            263] @?= 7

    , testCase "Day1-2 example" $
        numIncreasingTripletSums [
            199,
            200,
            208,
            210,
            200,
            207,
            240,
            269,
            260,
            263] @?= 5
    
    , testCase "pairs" $
        pairs [1, 2, 3, 4, 5] @?= [
            (1, 2),
            (2, 3),
            (3, 4),
            (4, 5)
        ]

    , testCase "triplets" $
        triplets [1, 2, 3, 4, 5] @?= [
            (1, 2, 3),
            (2, 3, 4),
            (3, 4, 5)
        ]

    , testCase "increases (1,2)" $
        increases (1,2) @? "should be increasing"

    , testCase "increases (2,1)" $
        (not $ increases (2,1)) @? "should be decreasing"
    ]
