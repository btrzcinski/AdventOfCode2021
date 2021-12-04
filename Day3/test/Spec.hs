import Lib (booleans, epsilonRate, gammaRate)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

day1Part1Sample :: [String]
day1Part1Sample =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "booleans" $
        booleans "10110" @?= [True, False, True, True, False],
      testCase "gammaRate for Day1-1 sample" $
        gammaRate day1Part1Sample @?= 22,
      testCase "epsilonRate for Day1-1 sample" $
        epsilonRate day1Part1Sample @?= 9
    ]