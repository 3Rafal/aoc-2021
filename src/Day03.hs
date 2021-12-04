module Day03 where

pt1 :: IO Int
pt1 = do
  txt <- readFile "input/Day03.txt"
  pure $ run $ lines txt

run :: [String] -> Int 
run = result . summary

data ColumnBits = ColumnBits
  { zeros :: Int
  , ones  :: Int
  }

emptyColumnBits :: ColumnBits
emptyColumnBits = ColumnBits 0 0

columnBits :: Int -> [String] -> ColumnBits
columnBits index = foldr (add . (!! index)) emptyColumnBits

mostCommon :: ColumnBits -> Char
mostCommon (ColumnBits z o) =
  if z > o then '0' else '1'

leastCommon :: ColumnBits -> Char
leastCommon (ColumnBits z o) =
  if z < o then '0' else '1'
  
type ReportSummary = [ColumnBits]

emptyReport :: [ColumnBits]
emptyReport = replicate 12 emptyColumnBits

add :: Char -> ColumnBits -> ColumnBits
add '0' (ColumnBits z o) = ColumnBits (z+1) o
add '1' (ColumnBits z o) = ColumnBits z (o+1)
add _ _ = undefined

countColumn :: String -> ReportSummary -> ReportSummary
countColumn = zipWith add

summary :: [String] -> ReportSummary
summary = foldl (flip countColumn) emptyReport

bitsToInt :: String -> Int
bitsToInt s = sum $ zipWith f (reverse [0..hiPow]) s
  where
    hiPow = length s - 1
    f :: Int -> Char -> Int
    f _ '0' = 0
    f i '1' = 2 ^ i
    f _ _ = undefined

gamma :: ReportSummary -> Int
gamma = bitsToInt . fmap mostCommon

epsilon :: ReportSummary -> Int
epsilon = bitsToInt . fmap leastCommon

result :: ReportSummary -> Int
result rs = gamma rs * epsilon rs

oxygen :: ColumnBits -> Char
oxygen (ColumnBits z o)
  | z == o = '1'
  | z < o  = '1'
  | z > o  = '0'
  
scrubber :: ColumnBits -> Char
scrubber (ColumnBits z o)
  | z == o = '0'
  | z < o  = '0'
  | z > o  = '1'
  
rating :: (ColumnBits -> Char) -> [String] -> Int
rating p strs =
  bitsToInt
  $ go strs [0..len]
  where
    go :: [String] -> [Int] -> String
    go [s] _ = s
    go ss (i:is) =
      let cbs = columnBits i ss in
      go (filter (predicate cbs i) ss) is
    go _ _ = undefined
      
    len :: Int
    len = length $ head strs

    predicate :: ColumnBits -> Int -> String -> Bool
    predicate cbs i str =
        p cbs == str !! i
    
pt2 :: IO Int
pt2 = do
  txt <- readFile "input/Day03.txt"
  pure $ (\ls -> rating oxygen ls * rating scrubber ls) $ lines txt
  
testInput :: [[Char]]
testInput =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]
