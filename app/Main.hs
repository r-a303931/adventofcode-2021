module Main where

import Data.Time.Clock
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Lib

runPart :: Show c => Int -> Solution a b c -> IO ()
runPart name part = do
  putStr ("Part " ++ show name ++ ": ")
  part2Start <- getCurrentTime
  runProgramP part
  part2End <- getCurrentTime
  let diff = diffUTCTime part2End part2Start
  putStr " ("
  putStr . show $ diff
  putStrLn ")"

day :: (Show c, Show f) => Int -> Solution a b c -> Solution d e f -> IO ()
day dayNumber part1 part2 = do
  putStrLn ("Day " ++ show dayNumber ++ "\n")

  runPart 1 part1
  runPart 2 part2

  putStrLn ""

day1 = day 1 Day1.part1 Day1.part2
day2 = day 2 Day2.part1 Day2.part2
day3 = day 3 Day3.part1 Day3.part2
day4 = day 4 Day4.part1 Day4.part2
day5 = day 5 Day5.part1 Day5.part2
day6 = day 6 Day6.part1 Day6.part2
day7 = day 7 Day7.part1 Day7.part2

main :: IO ()
main = do
  day1
  day2
  day3
  day4
  day5
  day6
  day7
