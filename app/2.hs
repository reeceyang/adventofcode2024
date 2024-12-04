module Main where

import Data.List.Split
import Data.List (sort)

inputFile :: String
inputFile = "inputs/2.txt"

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    fileContent <- readFile inputFile
    let rows = map (splitOn " ") $ lines fileContent
    print $ length $ filter (isSafe . map parseInt) rows

part2 :: IO ()
part2 = do
    fileContent <- readFile inputFile
    let rows = map (splitOn " ") $ lines fileContent
    print $ length $ filter (isSafe2 . map parseInt) rows

parseInt :: String -> Int
parseInt s = read s :: Int

dist :: Int -> Int -> Int
dist a b = abs (a - b)

isIncreasing :: [Int] -> Bool
isIncreasing rows = rows == sort rows

isDecreasing :: [Int] -> Bool
isDecreasing rows = rows == reverse (sort rows)

okDiffer :: [Int] -> Bool
okDiffer pair = (dist (head pair) (last pair)) <= 3 && (dist (head pair) (last pair)) >= 1

allOkDiffer :: [Int] -> Bool
allOkDiffer rows = (length $ filter id $ map okDiffer $ divvy 2 1 rows) == (length rows) - 1

isSafe :: [Int] -> Bool
isSafe rows = (isIncreasing rows || isDecreasing rows) && allOkDiffer rows

isSafe2 :: [Int] -> Bool
isSafe2 row = any (\x -> (isIncreasing x || isDecreasing x) && allOkDiffer x) $ splicedRows row

splicedRows :: [Int] -> [[Int]]
splicedRows row = map (\x -> (take x row) ++ (drop (x + 1) row)) [0..length row]