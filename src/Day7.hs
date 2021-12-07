module Day7 where

import Data.Function
import Data.List
import Lib
import Parser

test :: [Int]
test = [16,1,2,0,4,2,7,1,2,14]

results = makeTestT solve [ ( test, 37 ) ] ++ makeTestT solve2 [ ( test, 168 ) ]

memo   :: (Int -> Int) -> (Int -> Int)
memo f = (map f [0..] !!)

mCumSum :: Int -> Int
mCumSum = memo cumsum'
  where cumsum' v = sum [1..v]

fuelCost           :: [Int] -> Int -> Int
fuelCost crabs pos = sum . map (abs . (pos-)) $ crabs

fuelCost2           :: [Int] -> Int -> Int
fuelCost2 crabs pos = sum . map (mCumSum . abs . (pos-)) $ crabs

mean :: [Int] -> Double
mean = m . sort
  where m [v1]    = fromIntegral v1
        m [v1,v2] = fromIntegral (v1 + v2) / 2.0
        m v       = m . init . tail $ v

input :: IO String
input = readFile "inputs/day7.txt"

fileParser :: Parser [Int]
fileParser = sepBy (charP ',') intP

solve   :: [Int] -> Int
solve v = min (fuelCost v . floor . mean $ v) (fuelCost v . ceiling . mean $ v)

solve2   :: [Int] -> Int
solve2 v = fuelCost2 v . minimumBy (compare `on` (fuelCost2 v)) $ [1..(maximum v)]

part1 = Solution { filePathP="inputs/day7.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve
                 , displaySolutionP=id
                 }

part2 = part1 { solveProblemP=solve2 }
