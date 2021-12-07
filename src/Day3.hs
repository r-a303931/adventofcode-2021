module Day3 where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import Lib
import Parser

test :: [[Int]]
test = map p <$> [ "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010" ]
  where p c = read [c]


fileParser :: Parser [[Int]]
fileParser = many (lineParser <* charP '\n')
  where lineParser = map p <$> spanP isDigit
        p c = read [c]

results1 :: [TestResult (Int, Int)]
results1 = makeTestT solve1 [( test, (22, 9) )]

results2 :: [TestResult (Int, Int)]
results2 = makeTestT solve2 [( test, (23,10) )]

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (compare `on` length) . group . sort

reduce :: (Int, Int) -> Int
reduce = uncurry (*)

rot :: [[x]] -> [[x]]
rot = map reverse . transpose

reduceBits :: [Int] -> Int
reduceBits = foldl (\a b -> shift a 1 .|. b) 0

gammaBits     :: [[Int]] -> [Int]
gammaBits arr = map mostCommon . rot $ arr

solve1     :: [[Int]] -> (Int, Int)
solve1 arr = (reduceBits bits, reduceBits . map (1-) $ bits)
  where bits = gammaBits arr

part1 = Solution { filePathP="inputs/day3.txt"
                 , contentParser=fileParser
                 , solveProblemP=(reduce . solve1)
                 , displaySolutionP=id
                 }

oxygenBits         :: (Eq a, Ord a) => ([a] -> a) -> [[a]] -> [a]
oxygenBits ind arr = commonBit : if (length . head $ leftoverBits) == 0 then [] else oxygenBits ind leftoverBits
  where leftoverBits  = map tail filteredBytes
        filteredBytes = bitFilter arr
        bitFilter     = filter ((==commonBit) . head)
        commonBit     = ind . map head $ arr

solve2     :: [[Int]] -> (Int, Int)
solve2 arr = (reduceBits . (oxygenBits mostCommon) $ arr, reduceBits . (oxygenBits leastCommon) $ arr)

part2 = part1 { solveProblemP=(reduce . solve2) }
