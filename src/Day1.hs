module Day1 where

import Control.Applicative
import Lib
import Parser

getIncreases          :: [Int] -> [Ordering]
getIncreases (a:b:xs) = (if b > a then GT else LT) : getIncreases (b:xs)
getIncreases _        = []

solve :: [Int] -> Int
solve = length . filter (==GT) . getIncreases

input :: IO String
input = readFile "inputs/day1.txt"

fileParser :: Parser [Int]
fileParser = many (intP <* charP '\n')

part1 = Solution { filePathP="inputs/day1.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve
                 , displaySolutionP=id
                 }

getWindows            :: [Int] -> [Int]
getWindows (a:b:c:xs) = a+b+c : getWindows (b:c:xs)
getWindows _          = []

part2 = part1 { solveProblemP=(solve . getWindows) }
