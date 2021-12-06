module Day06 where

import           Data.List.Split (splitOn)
import           Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V

import Day05 (frequency)

run :: Int -> String -> Int
run n = sum . (!! n) . iterate day . populate . parse

pt1 :: IO Int
pt1 = do
  txt <- readFile "input/Day06.txt"
  pure $ run 80 txt

type Fishes = Vector Int

empty :: Fishes
empty = V.replicate 9 0

day :: Fishes -> Fishes
day fs = reset $ V.snoc t h
  where
    reset v = v // [(6,(v ! 6)  +h)]
    h = V.head fs
    t = V.tail fs

populate :: [Int] -> Fishes
populate x = empty // frequency x

parse :: String -> [Int]
parse = map read . splitOn ","
  
testInput :: String
testInput = "3,4,3,1,2"
