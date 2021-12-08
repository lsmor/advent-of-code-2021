module Main where
import Data.Attoparsec.ByteString.Char8
    ( isAlpha_ascii, takeWhile1, string, sepBy1, Parser, endOfLine )
import Data.Attoparsec.ByteString (Parser, parseOnly)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import qualified Data.Set as S
import Data.Set (Set, fromList)
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Control.Applicative (Applicative(liftA2))
import Data.Foldable ( Foldable(foldl') )
import Data.Maybe (fromMaybe)
import Debug.Trace

type Segment = Char
type Relation = [(Segment, Segment)]
type Digit = Set Segment
type Signal = [Digit]
data Display = Display {signals :: Signal, digit0 :: Digit, digit1 :: Digit, digit2 :: Digit, digit3 :: Digit} deriving (Eq, Show)

zero :: Digit
zero = S.fromList ['a', 'b', 'c', 'e', 'f', 'g']

one :: Digit
one = S.fromList ['c','f']

two :: Digit
two = S.fromList ['a', 'c', 'd', 'e', 'g']

three :: Digit
three = S.fromList ['a', 'c', 'd', 'f', 'g']

four :: Digit
four = S.fromList ['b', 'c', 'd', 'f']

five :: Digit
five = S.fromList ['a', 'b', 'd', 'f', 'g']

six :: Digit
six = S.fromList ['a', 'b', 'd', 'e', 'f', 'g']

seven :: Digit
seven = S.fromList ['a', 'c', 'f']

eigth :: Digit
eigth = S.fromList ['a', 'b', 'c', 'd', 'e', 'f', 'g']

nine :: Digit
nine = S.fromList ['a', 'b', 'c', 'd', 'f', 'g']

-- Logic 
findBaseOnSegments :: Int -> Signal -> Signal
findBaseOnSegments n = filter ( (== n) . S.size)

findOne, findFour, findSeven, findEigth  :: Signal -> Digit
findOne = head . findBaseOnSegments 2
findFour = head . findBaseOnSegments 4
findSeven = head . findBaseOnSegments 3
findEigth = head . findBaseOnSegments 7

-- Seven \\ One is the segment a
rule1 :: Signal -> Relation -> Relation
rule1 sig rel = (x, 'a'):rel
    where [x] = S.toList $ liftA2 S.difference findSeven findOne sig

-- Number two is the only number with no segment f
rule2 :: Signal -> Relation -> Relation
rule2 sig rel = (x, 'f'):rel
    where complements = fmap (M.fromList . S.toList . S.map ( , 1 :: Int) . S.difference (findEigth sig)) sig -- Super inefficient transform Set to List and then to Map because there is no unionWith function in Data.Set
          counts = foldl' (M.unionWith (+)) M.empty complements
          [(x,_)] = M.toList $ M.filter (==1) counts

-- Since we know 'f', we can deduce 'c' from number one
rule3 :: Signal -> Relation -> Relation
rule3 sig rel = (x, 'c'):rel
    where [(f, _)] = filter (('f'==) . snd ) rel
          [x] = S.toList $ S.delete f (findOne sig)

-- We know 4 inter 2 inter 3 inter 5 is d
rule4 :: Signal -> Relation -> Relation
rule4 sig rel = (x, 'd'):rel
  where [a,b,c] = findBaseOnSegments 5 sig
        d = findFour sig
        [x] = S.toList $ a `S.intersection` b `S.intersection` c `S.intersection` d

-- We now a, d, f. And number 4 is b, a, d, f. So 4 \\ relation = b
rule5 :: Signal -> Relation -> Relation
rule5 sig rel = (x, 'b'):rel
  where ks = S.fromList $ fmap fst rel
        [x] = S.toList $ S.difference (findFour sig) ks

-- 0 inter 9 inter 6 is abfg and wen can deduce g since we know the rest
rule6 :: Signal -> Relation -> Relation
rule6 sig rel = (x, 'g'):rel
  where [a,b,c] = findBaseOnSegments 5 sig
        ks = S.fromList $ fmap fst rel
        s = a `S.intersection` b `S.intersection` c
        [x] = S.toList $ s `S.difference` ks

-- One letter left.
rule7 :: Signal -> Relation -> Relation
rule7 sig rel = (x, 'e'):rel
  where s = findEigth sig
        ks = S.fromList $ fmap fst rel
        [x] = S.toList $ s `S.difference` ks

buildRelation :: Signal -> Relation
buildRelation sig = rule7 sig . rule6 sig . rule5 sig . rule4 sig . rule3 sig . rule2 sig . rule1 sig $ []

decode :: Digit -> Relation -> Digit
decode d rel = S.map (fromMaybe 'z' . (`lookup` rel)) d

digitToInt :: Digit -> Int
digitToInt d
  | d == zero = 0
  | d == one = 1
  | d == two = 2
  | d == three = 3
  | d == four = 4
  | d == five = 5
  | d == six = 6
  | d == seven = 7
  | d == eigth = 8
  | d == nine = 9
  | otherwise  = trace (show d) $ error "unexpected number"

decodeDisplay :: Display -> Int
decodeDisplay (Display sig d0 d1 d2 d3) = f d0 * 1000 + f d1 * 100 + f d2 * 10 + f d3
    where decoder = buildRelation sig
          f = digitToInt . (`decode` decoder)


-- Parser 
parseListOfDigits :: Parser [Digit]
parseListOfDigits = fmap (S.fromList . BS.unpack ) <$> takeWhile1 isAlpha_ascii `sepBy1` string " "

signalParser :: Parser Signal
signalParser = parseListOfDigits

digitParser :: Parser [Digit]
digitParser = parseListOfDigits

displayParser :: Parser Display
displayParser = do
    s <- signalParser
    string " | "
    [a,b,c,d] <- digitParser
    return $ Display s a b c d

inputParser :: Parser [Display]
inputParser = displayParser `sepBy1` endOfLine

--- >>> parseOnly displayParser "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
-- Right (Display {signals = [fromList "be",fromList "abcdefg",fromList "bcdefg",fromList "acdefg",fromList "bceg",fromList "cdefg",fromList "abdefg",fromList "bcdef",fromList "abcdf",fromList "bde"], digit0 = fromList "abcdefg", digit1 = fromList "bcdef", digit2 = fromList "bcdefg", digit3 = fromList "bceg"})

main :: IO ()
main = do
    contents <- BS.getContents
    let Right input = parseOnly inputParser contents
    putStrLn "Solution to part 1 is:"
    print $ sum $ length . filter (\s -> S.size s `elem` [2,3,4,7]) . (\d -> [digit0 d,digit3 d, digit2 d, digit3 d])  <$> input
    putStrLn "Solution to part 2 is:"
    print $ sum $ decodeDisplay <$> input
