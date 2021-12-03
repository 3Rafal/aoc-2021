module Day03 where

readInput :: IO Int
readInput = do
  txt <- readFile "input.txt"
  pure $ run $ lines txt

run :: [String] -> Int 
run = result . summary

data ColumnBits = ColumnBits
  { zeros :: Int
  , ones  :: Int
  }

emptyColumnBits :: ColumnBits
emptyColumnBits = ColumnBits 0 0

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
    f i '0' = 0
    f i '1' = 2 ^ i
    f _ _ = undefined

gamma :: ReportSummary -> Int
gamma = bitsToInt . fmap mostCommon

epsilon :: ReportSummary -> Int
epsilon = bitsToInt . fmap leastCommon

result :: ReportSummary -> Int
result rs = gamma rs * epsilon rs
    
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
