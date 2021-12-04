module Day04 where

import Data.List (find)
import Data.Matrix ( Matrix, fromLists, toLists, transpose )
import Data.Maybe (fromMaybe)

type Grid = Matrix (Maybe Int)

result :: Int -> Grid -> Int
result i g = i * foldl (\ a x -> a + fromMaybe 0 x) 0 g

won :: Grid -> Bool
won g = allCrossed g || allCrossed (transpose g)
  where
    allCrossed = any (all (== Nothing)) . toLists

crossOut :: Int -> Grid -> Grid
crossOut i =
  fmap (\x -> if pure i == x then Nothing else x)

parse :: String -> ([Int], [Grid])
parse s = (nums, grids)
  where
    h:t = lines s
    nums = map read $ split ',' h
    grids :: [Grid]
    grids =
      map fromLists
      $ group 5
      $ map (map (pure . read) . words)
      $ filter (/= "") t

-- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

-- https://hackage.haskell.org/package/ghc-9.2.1/docs/src/GHC.Utils.Misc.html#split
split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

bRound :: Int -> [Grid] -> (Maybe Int, [Grid])
bRound i gs = (res, crossed)
  where
    res = result i <$> find won crossed
    crossed = map (crossOut i) gs

run :: String -> Int
run s =
    go grids nums
  where
    go gs (n:ns) =
      case bRound n gs of
        (Just result,_) -> result
        (Nothing, newGs) -> go newGs ns
    (nums, grids) = parse s

pt1 :: IO Int
pt1 = do
  txt <- readFile "src/Day04.txt"
  pure $ run txt
  
bRound' :: Int -> [Grid] -> (Maybe Int, [Grid])
bRound' i gs =
    case (winner, crossed) of
      (Just w, [x]) -> (Just (result i w) , [x])
      (Just _, _)   -> (Nothing, removedWinner)
      (Nothing,_)   -> (Nothing, crossed)
  where
    removedWinner = filter ((/= winner) . pure) crossed
    winner = find won crossed
    crossed = map (crossOut i) gs
    
run' :: String -> Int
run' s =
    go grids nums
  where
    go gs (n:ns) =
      case bRound' n gs of
        (Just result,_) -> result
        (Nothing, newGs) -> go newGs ns
    (nums, grids) = parse s
    
pt2 :: IO Int
pt2 = do
  txt <- readFile "src/Day04.txt"
  pure $ run' txt
  
testInput :: String
testInput =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \6 10  3 18  5\n\
  \1 12 20 15 19\n\
  \\n\
  \3 15  0  2 22\n\
  \9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \2  0 12  3  7"

