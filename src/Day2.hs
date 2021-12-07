module Day2 where

import Control.Applicative
import Lib
import Parser

data SubmarineState1 = SubmarineState1 { ss1Depth :: Int
                                       , ss1Distance :: Int
                                       } deriving (Show)

data SubmarineState2 = SubmarineState2 { ss2Depth :: Int
                                       , ss2Distance :: Int
                                       , ss2Aim :: Int
                                       } deriving (Show)

data SubmarineCommand = Forward Int
                      | Down Int
                      | Up Int
                      deriving (Show)

lineParser :: Parser SubmarineCommand
lineParser = forward <|> down <|> up
  where forward = Forward <$> (stringP "forward " *> intP)
        down    = Down    <$> (stringP "down "    *> intP)
        up      = Up      <$> (stringP "up "      *> intP)

defaultSubmarineState1 :: SubmarineState1
defaultSubmarineState1 = SubmarineState1 { ss1Depth = 0, ss1Distance = 0 }

defaultSubmarineState2 :: SubmarineState2
defaultSubmarineState2 = SubmarineState2 { ss2Depth = 0, ss2Distance = 0, ss2Aim = 0 }

updateSubmarineState1                      :: SubmarineCommand -> SubmarineState1 -> SubmarineState1
updateSubmarineState1 (Forward dist) state = state { ss1Distance = (ss1Distance state + dist) }
updateSubmarineState1 (Down inDepth) state = state { ss1Depth = (ss1Depth state + inDepth) }
updateSubmarineState1 (Up inDepth)   state = state { ss1Depth = (ss1Depth state - inDepth) }

updateSubmarineState2                      :: SubmarineState2 -> SubmarineCommand -> SubmarineState2
updateSubmarineState2 state (Forward dist) = state { ss2Distance = (ss2Distance state + dist), ss2Depth = (ss2Depth state + ss2Aim state * dist) }
updateSubmarineState2 state (Down inDepth) = state { ss2Aim = (ss2Aim state + inDepth)}
updateSubmarineState2 state (Up inDepth)   = state { ss2Aim = (ss2Aim state - inDepth) }

reduceSubmarine1       :: SubmarineState1 -> Int
reduceSubmarine1 state = (ss1Depth state) * (ss1Distance state)

reduceSubmarine2       :: SubmarineState2 -> Int
reduceSubmarine2 state = (ss2Depth state) * (ss2Distance state)

part1 = Solution { filePathP="inputs/day2.txt"
                 , contentParser=(sepBy (charP '\n') lineParser)
                 , solveProblemP=(reduceSubmarine1 . foldr updateSubmarineState1 defaultSubmarineState1)
                 , displaySolutionP=id
                 }

part2 = part1 { solveProblemP=(reduceSubmarine2 . foldl updateSubmarineState2 defaultSubmarineState2) }
