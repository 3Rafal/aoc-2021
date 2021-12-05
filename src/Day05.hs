{-# LANGUAGE TupleSections #-}

module Day05 where

import Data.List.Split ( splitOn )
import Data.Map (fromListWith, toList)

run :: String -> Int
run = overlaps . allPos . parse

pt1 :: IO Int
pt1 = do
  txt <- readFile "input/Day05.txt"
  pure $ run txt

type Pos = (Int, Int)
type Line = (Pos, Pos)

overlaps :: [Pos] -> Int
overlaps = length . filter ((> 1) . snd) . frequency

-- https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs/22398506
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

allPos :: [Line] -> [Pos]
allPos = concatMap straightLine

range :: Int -> Int -> [Int]
range x x' = if x < x' then [x..x'] else [x'..x]

straightLine :: Line -> [Pos]
straightLine ((x,y),(x',y')) | x == x' = fmap (x,) (range y y')
straightLine ((x,y),(x',y')) | y == y' = fmap (,y) (range x x')
straightLine _ = []

parse :: String -> [Line]
parse = map line . lines
  where
    line = mapTuple tuple  . splitArrow
    tuple = mapTuple ((+ 1 ) . read) . splitOn ","
    mapTuple f x = (f (x !! 0), f (x !! 1))
    splitArrow = splitOn "->"

testInput :: String
testInput =
  "0,9 -> 5,9\n\
  \8,0 -> 0,8\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2"
