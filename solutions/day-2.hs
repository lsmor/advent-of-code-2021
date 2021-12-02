{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Foldable (Foldable(fold))
import Data.Attoparsec.ByteString.Char8
    ( parseOnly, string, endOfInput, Parser, decimal )
import Control.Applicative ((<|>), Applicative (liftA2))
import qualified Data.ByteString.Char8 as BS
import Control.Monad (foldM)

-- Logic 

data Point a = Point !a !a deriving (Eq, Show)
data Instruction = Forward Int | Down Int | Up Int deriving (Eq, Show)

x :: Point a -> a
x (Point a a') = a

y :: Point a -> a
y (Point a a') = a'

instance Num a => Semigroup (Point a) where
  (Point x y) <> (Point x' y') = Point (x + x') (y + y')

instance Num a => Monoid (Point a) where
    mempty  = Point 0 0


toPoint :: Instruction -> Point Int
toPoint (Forward n) = Point n 0
toPoint (Down n) = Point 0 n
toPoint (Up n) = Point 0 (-n)

calculatePosition :: [Instruction] -> Point Int
calculatePosition = foldMap toPoint

-- Parser

instructionParser :: Parser Instruction
instructionParser = parseForward <|> parseDown <|> parseUp
 where
  parseForward = Forward <$> (string "forward " *> decimal)
  parseDown = Down <$> (string "down " *> decimal)
  parseUp = Up <$> (string "up " *> decimal)


main :: IO ()
main = do
    contents <- BS.getContents
    let input = sequence $ parseOnly instructionParser <$> BS.split '\n' contents
    case input of 
      Left s -> print s
      Right ins -> do
          let result = calculatePosition ins 
          print result 
          print $ liftA2 (*) x y result 


