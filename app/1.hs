module Main where

import Data.List.Split
import Data.List (sort)

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    fileContent <- readFile "inputs/1.txt"
    let listA = sort $ map firstNum $ lines fileContent
    let listB = sort $ map secondNum $ lines fileContent
    print $ sum $ zipWith dist listA listB

part2 :: IO ()
part2 = do
    fileContent <- readFile "inputs/1.txt"
    let listA = map firstNum $ lines fileContent
    let listB = map secondNum $ lines fileContent
    print $ sum $ zipWith similarityScore listA $ map (countFrequency listB) listA

firstNum :: [Char] -> Int
firstNum line = read (head (splitOn "   " line)) :: Int

secondNum :: [Char] -> Int
secondNum line = read (last (splitOn "   " line)) :: Int

dist :: Int -> Int -> Int
dist a b = abs (a - b)

countFrequency :: [Int] -> Int -> Int
countFrequency list x = length $ filter (== x) list

similarityScore :: Int -> Int -> Int
similarityScore a b = a * b