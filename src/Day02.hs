{-# LANGUAGE ViewPatterns #-}

module Day02 where

import Data.List ( stripPrefix )

readInput :: IO Int
readInput = do
  txt <- readFile "Day02.txt"
  pure $ run $ lines txt

run :: [String] -> Int 
run = result . foldl (flip (move . parse)) start

data Move = Forward Int | Up Int | Down Int

parse :: String -> Move
parse (stripPrefix "forward" -> Just v) = Forward $ read v
parse (stripPrefix "up"      -> Just v) = Up      $ read v
parse (stripPrefix "down"    -> Just v) = Down    $ read v
parse _ = undefined

data Position = Position
  { depth      :: Int
  , horizontal :: Int
  , aim        :: Int
  }

start :: Position
start = Position 0 0 0

result :: Position -> Int
result (Position d h _) = d * h

move :: Move -> Position -> Position
move (Forward x) (Position d h a) = Position (d + a * x) (h + x) a
move (Up x)      (Position d h a) = Position d h (a - x)
move (Down x)    (Position d h a) = Position d h (a + x)

testInput :: [[Char]]
testInput =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ]
