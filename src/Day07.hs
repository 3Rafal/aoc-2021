module Day07 where

import Data.List (foldl')
import Data.List.Split ( splitOn )

type CostFun = Int -> Int -> Int

pt1 :: IO Int
pt1 = result cost1

pt2 :: IO Int
pt2 = result cost2

result :: CostFun -> IO Int
result f = do
  txt <- readFile "input/Day07.txt"
  pure $ brute f $ parse txt
  
brute :: CostFun -> [Int] -> Int
brute costFun crabs = minimum $ map res rng
  where
    rng = [minimum crabs..maximum crabs]
    res n = foldl' (\ acc x -> acc + costFun n x) 0 crabs

cost1 :: Int -> Int -> Int
cost1 n x = abs (n - x)

cost2 :: Int -> Int -> Int
cost2 n x = (delta * (delta+1)) `quot` 2
  where
    delta = cost1 n x

parse :: String -> [Int]
parse = map read . splitOn ","

testInput :: String
testInput = "16,1,2,0,4,2,7,1,2,14"
