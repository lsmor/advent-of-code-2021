module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable (foldl', sum)
import qualified Data.Map as M

fuelLinear :: [Int] -> Int -> Int
fuelLinear xs i = sum $ fmap ( abs . subtract i) xs

fuelIncrease :: [Int] -> Int -> Int
fuelIncrease xs i = sum $ fmap f xs
  where f x = let n = abs (x - i) in n*(n+1) `div` 2

solution :: [Int] -> ([Int] -> Int -> Int) -> Int
solution xs fuelCalculator = minimum $ fuelCalculator xs <$> [minimum xs .. maximum xs]

main :: IO ()
main = do 
    contents <- BS.getContents 
    let (Just input) = fmap fst <$> sequenceA (BS.readInt <$> BS.split ',' contents)    
    putStrLn "Solution to part 1 is:"
    print $ solution input fuelLinear
    putStrLn "Solution to part 2 is:"
    print $ solution input fuelIncrease