module Day01 where

import Data.List (zip4)

type Solution = [Int] -> Int

readInput :: Solution -> IO Int
readInput f = do
  txt <- readFile "input.txt"
  pure $ run f $ lines txt

run :: Solution -> [String] -> Int
run f = f . fmap read

-- part 1: How many measurements are larger than previous?
countLarger :: [Int] -> Int
countLarger xs =
    length $ filter isLarger $ zip xs (tail xs)
  where
    isLarger (prev, curr) = prev < curr

pt1 :: IO Int
pt1 = readInput countLarger

-- part 2: Sliding through 3 measures
countLarger' :: [Int] -> Int
countLarger' xs =
    length
    $ filter isLarger
    $ zip4 xs (tail xs) (tail $ tail xs) (tail $ tail $ tail xs)
  where
    isLarger (a,b,c,d) = a + b + c < b + c + d

pt2 :: IO Int
pt2 = readInput countLarger'

testInput =
  [ "199"
  , "200"
  , "208"
  , "210"
  , "200"
  , "207"
  , "240"
  , "269"
  , "260"
  , "263"
  ]
