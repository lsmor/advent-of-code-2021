module Main where
import Data.Attoparsec.ByteString.Char8
    ( parseOnly, string, Parser, decimal )
import Control.Applicative ((<|>), Applicative (liftA2))
import qualified Data.ByteString.Char8 as BS

-- Logic 

data Point a = Point !a !a deriving (Eq, Show)
data Control  = Control {position :: Point Int , aim :: Int} deriving (Eq, Show)
data Instruction = Forward Int | Down Int | Up Int deriving (Eq, Show)

getX :: Point a -> a
getX (Point a _) = a

getY :: Point a -> a
getY (Point _ a') = a'

instance Num a => Semigroup (Point a) where
  (Point x y) <> (Point x' y') = Point (x + x') (y + y')

instance Num a => Monoid (Point a) where
    mempty  = Point 0 0


instance Semigroup Control where
  Control (Point x y) a <> Control (Point x' y') a' =
    let x'' = x + x' 
        y'' = y + y' + x'*a 
        a'' = a + a'
    in  Control (Point x'' y'') a''

instance Monoid Control where
    mempty  = Control (Point 0 0) 0


toPoint :: Instruction -> Point Int
toPoint (Forward n) = Point n 0
toPoint (Down n) = Point 0 n
toPoint (Up n) = Point 0 (-n)

toControl :: Instruction -> Control
toControl (Forward n) = Control (Point n 0) 0
toControl (Down n) = Control (Point 0 0) n
toControl (Up n) = Control (Point 0 0) (-n)

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
          putStrLn  "Solution 1 is: "
          print $ liftA2 (*) getX getY (foldMap toPoint ins)
          putStrLn  "Solution 2 is: "
          print $ liftA2 (*) (getX . position) (getY . position) (foldMap toControl ins)

