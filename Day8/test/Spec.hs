import Lib
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

main :: IO ()
main = defaultMain unitTests

testData :: [String]
testData = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]

decodeTestData :: String
decodeTestData = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

unitTests :: TestTree
unitTests = testGroup
    "Unit tests" [
        testCase "numUniqueSegmentOutputDigits" $
            sum (map numUniqueSegmentOutputDigits testData) @?= 26,
        testCase "deriveDigitMap" $
            deriveDigitMap decodeTestData @?= Map.fromList [
                ("abcdefg", '8'),
                ("bcdef", '5'),
                ("acdfg", '2'),
                ("abcdf", '3'),
                ("abd", '7'),
                ("abcdef", '9'),
                ("bcdefg", '6'),
                ("abef", '4'),
                ("abcdeg", '0'),
                ("ab", '1')
            ],
        testCase "decodeOutputValue" $
            decodeOutputValue decodeTestData @?= 5353,
        testCase "decodeOutputValue (example data sum)" $
            sum (map decodeOutputValue testData) @?= 61229
    ]