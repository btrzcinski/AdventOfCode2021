import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

testData :: [Int]
testData = [16,1,2,0,4,2,7,1,2,14]

unitTests :: TestTree
unitTests = testGroup
    "Unit tests" [
        testCase "minStraightLineFuelForAlignment (part 1 example)" $
            minStraightLineFuelForAlignment testData @?= 37
    ]