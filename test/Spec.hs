import qualified Data.Map.Strict as M

import           DayEightTest
import           DayElevenTest
import           DayFiveTest
import           DayFourTest
import           DayNineTest
import           DayOneTest
import           DaySevenTest
import           DaySixTest
import           DayTenTest
import           DayThirteenTest
import           DayThreeTest
import           DayTwelveTest
import           DayTwoTest
import           Test.HUnit
import Data.Char (toLower)

main :: IO Counts
main = runTestTT $ TestList tests


fullSuite :: Test
fullSuite = undefined


tests :: [Test]
tests = [ test_D1
        , test_D2
        , test_D3
        , test_D4
        , test_D5
        , test_D6
        , test_D7
        , test_D8
        , test_D9
        , test_D10
        , test_D11
        , test_D12
        , test_D13
        ]

stringToNum :: String -> Maybe Int
stringToNum s = M.lookup (map toLower s) $ M.fromList [ ("one", 0)
                                                  , ("two", 1)
                                                  , ("three", 2)
                                                  , ("four", 3)
                                                  , ("five", 4)
                                                  , ("six", 5)
                                                  , ("seven", 6)
                                                  , ("eight", 7)
                                                  , ("nine", 8)
                                                  , ("ten", 9)
                                                  , ("eleven", 10)
                                                  , ("twelve", 11)
                                                  , ("thirteen", 12)
                                                  ]
