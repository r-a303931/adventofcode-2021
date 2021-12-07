module Day6 where

import Lib
import Parser

type Map = [[Maybe Int]]

testSeed :: [Int]
testSeed = [3,4,3,1,2]

results = makeTestT (snd . solve 18) [ ( testSeed, 26 ) ] ++ makeTestT (snd . solve 80) [ ( testSeed, 5934 ) ]

emptyMap :: Map
emptyMap = repeat . take 9 . repeat $ Nothing

replace :: Int -> Int -> Int -> Map -> Map
replace row col v fishMap = prevRows ++ [newRow] ++ otherRows
  where prevRows  = take row fishMap
        newRow    = take col (fishMap !! row) ++ [Just v] ++ drop (col + 1) (fishMap !! row)
        otherRows = drop (row + 1) fishMap

handleSingleFish                    :: Int -> Int -> Map -> (Map, Int)
handleSingleFish timer fish fishMap = case fishMap !! timer !! fish of
  Just v  -> (fishMap, v)
  Nothing -> (newMap, computedValue)
    where newMap                       = replace timer fish computedValue computedMap
          (computedMap, computedValue) = singleLanternFish (timer - fish) 0 fishMap

handleTwoFish               :: Int -> Map -> (Map, Int)
handleTwoFish timer fishMap = (fishMap'', firstFishCount + secondFishCount)
  where (fishMap'', secondFishCount) = singleLanternFish (timer - 1) 8 fishMap'
        (fishMap',  firstFishCount)  = singleLanternFish (timer - 1) 6 fishMap

singleLanternFish :: Int -> Int -> Map -> (Map, Int)
singleLanternFish timer fish fishMap = if timer < 7 && fish >= timer
  then (fishMap, 1)
  else
        if fish == 0
                then handleTwoFish timer fishMap
                else handleSingleFish timer fish fishMap

solve                     :: Int -> [Int] -> (Map, Int)
solve iterationCount seed = foldl f (emptyMap, 0) seed
  where f (fishMap,currentTotal) fishTimer = (newMap,newTotalCount)
          where newTotalCount            = currentTotal + fishCount
                (newMap,fishCount)       = singleLanternFish iterationCount fishTimer fishMap

part1 = ProgramP { filePathP="inputs/day6.txt"
                 , contentParser=(sepBy (charP ',') intP)
                 , solveProblemP=(snd . solve 80)
                 , displaySolutionP=id
                 }

part2 = part1 { solveProblemP=(snd . solve 256) }
