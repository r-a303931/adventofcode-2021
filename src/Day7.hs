module Day7 where

import Control.Monad
import Data.Function
import Data.List
import Lib
import Parser

test :: [Int]
test = [16,1,2,0,4,2,7,1,2,14]

-- results = makeTestT solve [ (test, 37) ]

memo   :: (Int -> Int) -> (Int -> Int)
memo f = (map f [0..] !!)

mCumSum :: Int -> Int
mCumSum = memo cumsum'
  where cumsum' v = sum [1..v]

fuelCost           :: [Int] -> Int -> Int
fuelCost crabs pos = sum . map (abs . (pos-)) $ crabs

fuelCost2           :: [Int] -> Int -> Int
fuelCost2 crabs pos = sum . map (mCumSum . abs . (pos-)) $ crabs

average   :: [Int] -> (Int, Int)
average v = (floor avg, ceiling avg)
  where avg = (tot / (fromIntegral . length $ vs)) :: Double
        tot = sum . map fromIntegral $ vs
        vs  = filter (not . isOutlier v) $ v

average'   :: [Int] -> (Int, Int)
average' v = (floor avg, ceiling avg)
  where avg = (tot / (fromIntegral . length $ v)) :: Double
        tot = sum . map fromIntegral $ v

mean :: [Int] -> Double
mean = m . sort
  where m [v1]    = fromIntegral v1
        m [v1,v2] = fromIntegral (v1 + v2) / 2.0
        m v       = m . init . tail $ v

splitAtMean   :: [Int] -> ([Int], [Int])
splitAtMean v = (l, m)
  where l      = takeWhile ((< cMean) . fromIntegral) sorted
        m      = dropWhile ((\n -> n == cMean || n < cMean) . fromIntegral) sorted
        sorted = sort v
        cMean  = mean v

q1 :: [Int] -> Double
q1 = mean . fst . splitAtMean

q3 :: [Int] -> Double
q3 = mean . snd . splitAtMean

iqr :: [Int] -> Double
iqr = liftM2 (-) q3 q1

isOutlier      :: [Int] -> Int -> Bool
isOutlier vs v = fromIntegral v < low || fromIntegral v > high
  where high = q3 vs + 1.5 * iqr vs
        low  = q1 vs - 1.5 * iqr vs

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
