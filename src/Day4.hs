module Day4 where

import Control.Applicative
import Data.List
import Data.Maybe
import Lib
import Parser

data Space = Unmarked Int
           | Marked   Int
           deriving (Show)

type Board = [[Space]]

testMoves :: [Int]
testMoves = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

testBoards :: [Board]
testBoards = map shapeBoard [ [22,13,17,11,0,8,2,23,4,24,21,9,14,16,7,6,10,3,18,5,1,12,20,15,19]
                            , [3,15,0,2,22,9,18,13,17,5,19,8,7,25,23,20,11,10,24,4,14,21,16,12,6]
                            , [14,21,17,24,4,10,16,15,9,19,18,8,23,26,20,22,11,13,6,5,2,0,12,3,7]
                            ]

shapeBoard :: [Int] -> Board
shapeBoard = map (map Unmarked) . reshape 5

lineParser :: Parser [Int]
lineParser = sepBy (charP ',') intP

boardParser :: Parser Board
boardParser = shapeBoard <$> (sepBy (charP ',') intP <* charP '\n')

fileParser :: Parser ([Int], [Board])
fileParser = pure (,) <*> (lineParser <* charP '\n') <*> many boardParser

isChecked              :: Space -> Bool
isChecked (Unmarked _) = False
isChecked (Marked _)   = True

checkCell                :: Int -> Space -> Space
checkCell a (Unmarked b) = if a == b then Marked a else Unmarked b
checkCell _ b            = b

rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse

reshape        :: Int -> [a] -> [[a]]
reshape _ []   = []
reshape c vals = [take c vals] ++ reshape c (drop c vals)

isWinningBoard       :: Board -> Bool
isWinningBoard board = horizontalWins || verticalWins
  where horizontalWins = any (all isChecked)        $ board
        verticalWins   = any (all isChecked) . rotl $ board

markCell      :: Int -> Board -> Board
markCell cell = map (map (checkCell cell))

stepCount                    :: Int -> [Int] -> Board -> (Int, Board, Int)
stepCount steps (x:xs) board = if isWinningBoard newBoard
  then (x, newBoard, steps + 1)
  else stepCount (steps + 1) xs newBoard
  where newBoard = markCell x board

markedCells :: Board -> [Int]
markedCells = (>>= (mapMaybe f))
  where f (Unmarked _) = Nothing
        f (Marked v)   = Just v

reduceBoard         :: (Int, Board, Int) -> Int
reduceBoard (a,b,_) = a * sum (b >>= mapMaybe f)
  where f (Unmarked v) = Just v
        f (Marked _)   = Nothing

solve1                :: ([Int], [Board]) -> (Int, Board, Int)
solve1 (moves,boards) = head . sortOn (\(_,_,c) -> c) . map (stepCount 0 moves) $ boards

solve2                :: ([Int], [Board]) -> (Int, Board, Int)
solve2 (moves,boards) = last . sortOn (\(_,_,c) -> c) . map (stepCount 0 moves) $ boards

input = getInput "inputs/day4.txt" fileParser

part1 = Solution { filePathP="inputs/day4.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=reduceBoard
                 }

part2 = part1 { solveProblemP=solve2 }
