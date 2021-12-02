import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Forward units" $
        units (Forward 3) @?= 3
    , testCase "Down units" $
        units (Down 2) @?= 2
    , testCase "Up units" $
        units (Up 1) @?= 1
    , testCase "parseLineToCommand forward" $
        parseLineToCommand "forward 2" @?= Forward 2
    , testCase "parseLineToCommand down" $
        parseLineToCommand "down 1" @?= Down 1
    , testCase "parseLineToCommand up" $
        parseLineToCommand "up 3" @?= Up 3
    , testCase "delta forward" $
        delta (Forward 3) @?= (3, 0)
    , testCase "delta up" $
        delta (Up 2) @?= (0, -2)
    , testCase "delta down" $
        delta (Down 1) @?= (0, 1)
    , testCase "finalPosition" $
        finalPosition [(Forward 1), (Down 2), (Up 1)] @?= (1, 1)
    , testCase "Day2-1" $
        finalPosition (map parseLineToCommand [
            "forward 5",
            "down 5",
            "forward 8",
            "up 3",
            "down 8",
            "forward 2"
        ]) @?= (15, 10)
    , testCase "Day2-1 score" $
        (uncurry (*)) (15, 10) @?= 150
    , testCase "Day2-2" $
        finalAimPosition [(Forward 5), (Down 5), (Forward 8), (Up 3), (Down 8), (Forward 2)]
        @?= (15, 60)
    , testCase "Day2-2 score" $
        (uncurry (*)) (15, 60) @?= 900
    ]
