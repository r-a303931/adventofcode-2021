module Main where

import Data.Time.Clock
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Lib

day :: Int -> IO () -> IO () -> IO ()
day dayNumber part1 part2 = do
  putStrLn ("Day " ++ show dayNumber ++ "\n")

  putStr "Part 1: "

  part1Start <- getCurrentTime
  part1
  part1End <- getCurrentTime
  let diff = diffUTCTime part1End part1Start
  putStr " ("
  putStr . show $ diff
  putStrLn ")"

  putStr "Part 2: "
  part2Start <- getCurrentTime
  part2
  part2End <- getCurrentTime
  let diff = diffUTCTime part2End part2Start
  putStr " ("
  putStr . show $ diff
  putStrLn ")"

  putStrLn ""

day1 = day 1 Day1.part1 Day1.part2
day2 = day 2 Day2.part1 Day2.part2
day3 = day 3 Day3.part1 Day3.part2
day4 = day 4 (runProgram Day4.part1) (runProgram Day4.part2)
day5 = day 5 (runProgram Day5.part1) (runProgram Day5.part2)
day6 = day 6 (runProgramP Day6.part1) (runProgramP Day6.part2)

main :: IO [()]
main = sequenceA [day1, day2, day3, day4, day5, day6]
