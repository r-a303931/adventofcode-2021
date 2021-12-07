module Lib where

import Data.Maybe
import Debug.Trace
import Parser

mTrace :: (a -> String) -> a -> a
mTrace = (trace =<<)

data TestResult a = Success | Failure (a, a)
  deriving (Show)

testCase          :: Eq b => (a -> b) -> (a, b) -> TestResult b
testCase f (a, b) = if res == b then Success else Failure (res, b)
  where res = f a

makeTest          :: Eq b => (a -> b) -> ([a], [b]) -> [TestResult b]
makeTest f (a, b) = map (testCase f) $ zip a b


makeTestT :: Eq b => (a -> b) -> [(a, b)] -> [TestResult b]
makeTestT = map . testCase

solveHandlerTemplate                :: ([String] -> String) -> [String] -> [String]
solveHandlerTemplate _ []           = []
solveHandlerTemplate f (line:otherLines) = f (take lineCount otherLines) : solveHandlerTemplate f (drop lineCount otherLines)
  where lineCount = read line :: Int

data Program a b c = Program { filePath :: String
                             , parseContents :: String -> a
                             , solveProblem :: a -> b
                             , displaySolution :: b -> c
                             }

data ProgramP a b c = ProgramP { filePathP :: String
                               , contentParser :: Parser a
                               , solveProblemP :: a -> b
                               , displaySolutionP :: b -> c
                               }

runProgramP         :: Show c => ProgramP a b c -> IO ()
runProgramP program = interactFile (filePathP program) $ fromJust . (\s -> ((displaySolutionP program) . (solveProblemP program) . fst) <$> (parse (contentParser program) s))

runProgram         :: Show c => Program a b c -> IO ()
runProgram program = interactFile (filePath program) $ (displaySolution program) . (solveProblem program) . (parseContents program)

interactFile      :: Show a => String -> (String -> a) -> IO ()
interactFile fp f = (show . f <$> readFile fp) >>= putStr

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
