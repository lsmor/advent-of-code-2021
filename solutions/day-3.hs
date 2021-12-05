module Main where

import Data.Monoid
import Data.Foldable (foldl')
import Data.Char
import Numeric

{-
The matrix 

00100
11110
10110

is represented as a nested list of elements of the Count Monoid. The function digit2Count injects an binary integer into the monoid
This is not a very good data structure, but enough for AoC

[ [(Sum 1, Sum 0), (Sum 1, Sum 0), (Sum 0, Sum 1), (Sum 1, Sum 0), (Sum 1, Sum 0)]
, [(Sum 0, Sum 1), (Sum 0, Sum 1), (Sum 0, Sum 1), (Sum 0, Sum 1), (Sum 1, Sum 0)]
, [(Sum 0, Sum 1), (Sum 1, Sum 0), (Sum 0, Sum 1), (Sum 0, Sum 1), (Sum 1, Sum 0)]
]

-}

type Count = (Sum Int, Sum Int)

-- Converts an integer into Count
digit2Count :: Int -> Count
digit2Count i = if i == 0 then (Sum 1, Sum 0) else (Sum 0, Sum 1)

-- Inverse of digit2Count
count2Digit :: Count -> String
count2Digit (Sum 1, Sum 0) = "0"
count2Digit (Sum 0, Sum 1) = "1"
count2Digit _ = ""

count2Digits :: Count -> (String, String)
count2Digits (zero, one) = if zero >= one then ("0", "1") else ("1", "0")

binaryToDecimal :: String -> Int
binaryToDecimal = fst . head . readInt 2 (\c -> c == '0' || c == '1') digitToInt

foldCounts :: [[Count]] -> [Count]
foldCounts [] = []
foldCounts (x:xs) = foldl' (zipWith (<>)) x xs

filterCounts :: ([[Count]], [[Count]]) -> Int -> (Int, Int)
filterCounts ([], []) _  = (0,0)
filterCounts ([mostcommon], [leastcommon]) _  = (binaryToDecimal $ foldMap count2Digit mostcommon, binaryToDecimal $ foldMap count2Digit leastcommon)
filterCounts (mostcommons, leastcommons) n = 
  let new_mostcommons =
        case mostcommons of
          []  -> mostcommons
          [m] -> mostcommons
          _ ->
            case compare zero one of
              LT -> ones mostcommons
              EQ -> ones mostcommons
              GT -> zeros mostcommons
      new_leastcommons =
        case leastcommons of
          []  -> leastcommons
          [l] -> leastcommons
          _ ->
            case compare zero' one' of
              LT -> zeros leastcommons
              EQ -> zeros leastcommons
              GT -> ones leastcommons
  in filterCounts (new_mostcommons, new_leastcommons) (n+1)
 where (zero, one)   = foldCounts mostcommons !! n
       (zero', one') = foldCounts leastcommons !! n
       zeros = filter (\e -> e !! n == (Sum 1, Sum 0))
       ones  = filter (\e -> e !! n == (Sum 0, Sum 1))

main :: IO ()
main = do
  contents <- words <$> getContents                            -- Get Content as list of strings
  let input = fmap (digit2Count . digitToInt) <$> contents     -- Transform from raw text, to Count
      counts = foldCounts input                                -- Folds row wise the matrix.
      (gamma, epsilon) = foldMap count2Digits counts
      (oxigen, co2) = filterCounts (input, input) 0
  putStrLn "Solution to part 1 is:"
  print $ binaryToDecimal gamma * binaryToDecimal epsilon
  putStrLn "Solution to part 2 is:"
  print $ oxigen * co2
  