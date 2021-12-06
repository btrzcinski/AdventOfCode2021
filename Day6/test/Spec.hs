import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

testData :: [Int]
testData = [3,4,3,1,2]

unitTests :: TestTree
unitTests = testGroup
    "Unit tests" [
        testCase "numFishAfter (part 1 example) - 18 days" $
            numFishAfter testData 18 @?= 26,
        testCase "numFishAfter (part 1 example) - 80 days" $
            numFishAfter testData 80 @?= 5934,
        testCase "numFishAfter (part 1 example) - 256 days" $
            numFishAfter testData 256 @?= 26984457539
    ]