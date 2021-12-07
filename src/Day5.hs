module Day5 where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Lib
import Parser

type Point = (Int, Int)
type Line  = (Point, Point)
type Map   = [[Int]]

testLines :: [Line]
testLines = [ ((0,9), (5,9))
            , ((8,0), (0,8))
            , ((9,4), (3,4))
            , ((2,2), (2,1))
            , ((7,0), (7,4))
            , ((6,4), (2,0))
            , ((0,9), (2,9))
            , ((3,4), (1,4))
            , ((0,0), (8,8))
            , ((5,5), (8,2))
            ]

results = makeTestT solve1 [ ( testLines, 5 ) ] ++ makeTestT solve2 [ ( testLines, 12 ) ]

horizontalOrVerticalLine                    :: Line -> Bool
horizontalOrVerticalLine ((x1,y1), (x2,y2)) = (x1 == x2) || (y1 == y2)

maxPoint :: [Line] -> Point
maxPoint = liftM2 (,) maxXCoordinate maxYCoordinate
  where maxXCoordinate = foldl max 0 . ((\((x1,_),(x2,_)) -> [x1,x2]) =<<)
        maxYCoordinate = foldl max 0 . ((\((x1,_),(x2,_)) -> [x1,x2]) =<<)

emptyMap :: Map
emptyMap = repeat (repeat 0)

directionalRange     :: Int -> Int -> [Int]
directionalRange x y = if x > y
  then reverse [y..x]
  else [x..y]

lineToPoints                   :: Line -> [Point]
lineToPoints ((x1,y1),(x2,y2)) = case ((x1 == x2), (y1 == y2)) of
  (True, _) -> [ (x1,y) | y <- [(min y1 y2)..(max y1 y2)] ]
  (_, True) -> [ (x,y1) | x <- [(min x1 x2)..(max x1 x2)] ]
  _         -> zipWith (,) (directionalRange x1 x2) (directionalRange y1 y2)

updateIndex                   :: Int -> (a -> a) -> [a] -> [a]
updateIndex index update list = take index list ++ [update (list !! index)] ++ drop (index + 1) list

updatePoint           :: (Int -> Int) -> Point -> Map -> Map
updatePoint val (x,y) = updateIndex y (updateIndex x val)

findMap :: [Line] -> Map
findMap = foldr (updatePoint (+1)) emptyMap . (>>= lineToPoints)

getHotPoints       :: Point -> Map -> Int
getHotPoints (x,y) = foldl (\s l -> s + (foldl (\c v -> if v >= 2 then c + 1 else c) 0 $ take y l)) 0 . take x

input :: IO [Line]
input = (fromJust . fmap fst . parse fileParser) <$> readFile "inputs/day5.txt"

fileParser :: Parser [Line]
fileParser = many (lineParser <* ws)
  where lineParser    = (pure l <*> (intP <* charP ',') <*> (intP <* stringP " -> ") <*> (intP <* charP ',') <*> intP)
        l x1 y1 x2 y2 = ((x1,y1),(x2,y2))

solve1 cLines = getHotPoints (maxPoint cLines) . findMap . filter horizontalOrVerticalLine $ cLines
part1 = Solution { filePathP="inputs/day5.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

solve2 cLines = getHotPoints (maxPoint cLines) . findMap $ cLines
part2 = part1 { solveProblemP=solve2 }
