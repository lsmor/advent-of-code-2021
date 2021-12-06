module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
    ( decimal,
      satisfy,
      skipWhile,
      endOfLine,
      parseOnly,
      string,
      many1,
      sepBy,
      Parser )
import Data.Attoparsec.Combinator ( many1, sepBy )
import Data.Matrix ( Matrix )
import qualified Data.Matrix as M
import Control.Applicative ((<|>), optional)
import qualified Data.Char as C

type Entry = (Int, Bool)
type Board = Matrix Entry
data Input = Input {header :: [Int], boards :: [Board]} deriving (Show)

-- Logic
boardIsWinning :: Board -> Bool
boardIsWinning b = winningRow || winningCol
 where
    winningRow = or $ and <$> M.toLists (snd <$> b)
    winningCol = or $ and <$> M.toLists (snd <$> M.transpose b)

boardScore :: Board -> Int
boardScore b = sum $ M.toList (entryScore <$> b)
 where entryScore (n, m) =  n * fromEnum (not m)

updateBoard :: Int -> Board -> Board
updateBoard e b = updateEntry <$> b
 where updateEntry (n, b) = if n == e then (n, True) else (n, b)

win :: Input -> Int
win (Input [] bs) = 0
win (Input (x:xs) bs) =
    case wins of
        []    -> win $ Input xs markedBoards
        (w:_) -> x * boardScore w
 where markedBoards = updateBoard x <$> bs
       wins = filter boardIsWinning markedBoards


loose :: Input -> Int
loose = go ((, False) <$> M.zero 5 5, 0)
  where
    go (acc, n) (Input []      _) = n * boardScore acc
    go (acc, n) (Input (x:xs) bs) =
        let markedBoards = updateBoard x <$> bs
            (w, l) = span boardIsWinning markedBoards
            s = n * boardScore acc
        in case w of
             []    -> go (acc, n) (Input xs l)
             (w:_) -> go (w, x) (Input xs l)


-- Parser
emptyLine :: Parser ()
emptyLine = endOfLine

isWhiteSpace :: Char -> Bool
isWhiteSpace = (== ' ')

whiteSpace :: Parser Char
whiteSpace = satisfy isWhiteSpace

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipWhile isWhiteSpace

headerParser :: Parser [Int]
headerParser = decimal `sepBy` string ","

lineParser :: Parser [Entry]
lineParser = fmap (, False) <$>  many1 (skipWhiteSpace *> decimal)

boardParser :: Parser Board
boardParser = M.fromLists <$> many1 (lineParser <* optional endOfLine)

inputParser :: Parser Input
inputParser = do
    h <- headerParser <* endOfLine
    emptyLine
    bs  <- boardParser `sepBy` endOfLine
    return $ Input h bs

main :: IO ()
main = do
    contents <- BS.getContents
    let Right input = parseOnly inputParser contents
    putStrLn "Solution to part 1 is:"
    print $ win input
    putStrLn "Solution to part 2 is:"
    print $ loose input


