module Day1 where

getIncreases          :: [Int] -> [Ordering]
getIncreases (a:b:xs) = (if b > a then GT else LT) : getIncreases (b:xs)
getIncreases _        = []

solve :: [Int] -> Int
solve = length . filter (==GT) . getIncreases

part1 :: IO ()
part1 = ((show . solve . map read . lines) <$> readFile "inputs/day1.txt") >>= putStr

getWindows            :: [Int] -> [Int]
getWindows (a:b:c:xs) = a+b+c : getWindows (b:c:xs)
getWindows _          = []

part2 :: IO ()
part2 = ((show . solve . getWindows . map read . lines) <$> readFile "inputs/day1.txt") >>= putStr
