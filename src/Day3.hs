module Day3 where

import Data.Bits
import Data.Function
import Data.List
import Lib

test :: [String]
test = [ "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010" ]

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

convertBit   :: Char -> Int
convertBit c = read [c]

reduceBits :: [Int] -> Int
reduceBits = foldl (\a b -> shift a 1 .|. b) 0

gammaBits     :: [[Char]] -> [Int]
gammaBits arr = map mostCommon . convertBits . rot $ arr

solve1     :: [[Char]] -> (Int, Int)
solve1 arr = (reduceBits bits, reduceBits . map (1-) $ bits)
  where bits = gammaBits arr

part1 :: IO ()
part1 = interactFile "inputs/day3.txt" (reduce . solve1 . lines)

convertBits :: [[Char]] -> [[Int]]
convertBits = map (map convertBit)

oxygenBits         :: (Eq a, Ord a) => ([a] -> a) -> [[a]] -> [a]
oxygenBits ind arr = commonBit : if (length . head $ leftoverBits) == 0 then [] else oxygenBits ind leftoverBits
  where leftoverBits  = map tail filteredBytes
        filteredBytes = bitFilter arr
        bitFilter     = filter ((==commonBit) . head)
        commonBit     = ind . map head $ arr

solve2     :: [[Char]] -> (Int, Int)
solve2 arr = (reduceBits . (oxygenBits mostCommon) . convertBits $ arr, reduceBits . (oxygenBits leastCommon) . convertBits $ arr)

part2 :: IO ()
part2 = interactFile "inputs/day3.txt" (reduce . solve2 . lines)
