module Main where

import qualified Data.Map.Strict as M
import Data.Attoparsec.ByteString.Char8 ( decimal, string, endOfLine, sepBy1, parseOnly )
import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BS
import Data.Foldable ( Foldable(foldl') )

data Point a = Point !a !a deriving (Eq, Show, Ord)
data Line a = Line {beginning :: !(Point a), end :: !(Point a)} deriving (Eq, Show, Ord)
type Counter = M.Map (Point Int) Int


-- Logic
lineToCounter :: Line Int -> Counter
lineToCounter (Line (Point x y) (Point x' y'))
    | x == x' = M.fromList [(Point x i, 1 )| i <- [min y y' .. max y y']]
    | y == y' = M.fromList [(Point i y, 1) | i <- [min x x' .. max x x']]
    | otherwise = M.empty 

lineToCounterWithDiagonals :: Line Int -> Counter
lineToCounterWithDiagonals (Line (Point x y) (Point x' y'))
    | x == x' = M.fromList [(Point x i, 1 )| i <- range y y']
    | y == y' = M.fromList [(Point i y, 1) | i <- range x x']
    | abs(x - x') == abs(y - y') = M.fromList [(Point i j, 1) | (i, j) <- zip (range x x') (range y y')]
    | otherwise = M.empty 
 where range a b = case compare a b of
         LT -> enumFromThenTo a (succ a) b
         EQ -> [a]
         GT -> enumFromThenTo a (pred a) b 

mergeCounter :: [Counter] -> Counter
mergeCounter = foldl' (M.unionWith (+)) M.empty

-- Parser
pointParser :: Parser (Point Int)
pointParser = do
    x <- decimal
    string ","
    y <- decimal
    return $ Point x y

lineParser :: Parser (Line Int)
lineParser = do
    p0 <- pointParser
    string " -> "
    p1 <- pointParser
    return $ Line p0 p1

inputParser :: Parser [Line Int]
inputParser = lineParser `sepBy1` endOfLine 
    


main :: IO ()
main = do
    contents <- BS.getContents 
    let Right input = parseOnly inputParser contents
    putStrLn "Solution to part 1 is:"
    print $ M.size $ M.filter (>1) $ mergeCounter (lineToCounter <$> input)
    putStrLn "Solution to part 2 is:"
    print $ M.size $ M.filter (>1) $ mergeCounter (lineToCounterWithDiagonals <$> input)