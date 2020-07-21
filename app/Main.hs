module Main where

import System.Environment (getArgs)

import DayOne
import DayTwo
import DayThree
import DayFour
import DayFive
import DaySix
import DaySeven
import DayEight
import DayNine
import DayTen
import DayEleven
import DayTwelve
import DayThirteen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> answersD13
    ["test"] -> runTests
    ["all"]  -> runAll
    x        -> parseArg x

runTests :: IO ()
runTests = undefined

parseArg :: [String] -> IO ()
parseArg = undefined

runAll :: IO ()
runAll = undefined
