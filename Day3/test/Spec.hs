import Lib (booleans, co2ScrubberRating, epsilonRate, gammaRate, lifeSupportRating, oxygenGeneratorRating, powerConsumption)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

sampleData :: [String]
sampleData =
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
        gammaRate sampleData
      
          @?= 22,
      testCase "epsilonRate for Day1-1 sample" $
        epsilonRate sampleData
      
          @?= 9,
      testCase "powerConsumption for Day1-1 sample" $
        powerConsumption sampleData
      
          @?= 198,
      testCase "oxygenGeneratorRating for Day1-2 sample" $
        oxygenGeneratorRating sampleData
      
          @?= 23,
      testCase "co2ScrubberRating for Day1-2 sample" $
        co2ScrubberRating sampleData
       @?= 10,
      testCase "lifeSupportRating for Day1-2 sample" $
        lifeSupportRating sampleData
       @?= 230
    ]