module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable (foldl', sum)
import qualified Data.Map as M

type Timer = Int

update :: M.Map Timer Int -> M.Map Timer Int
update m = case M.lookup 0 m of
  Nothing -> M.mapKeysWith (+) f m
  Just a -> M.insertWith (+) 8 a $ M.mapKeysWith (+) f m
 where f n = if n == 0 then 6 else n - 1

inputMap :: M.Map Timer Int
inputMap = M.fromListWith (+) $ zip [3,4,3,1,2] [1,1..]


main :: IO ()
main = do 
    contents <- BS.getContents 
    let (Just input) = fmap fst <$> sequenceA (BS.readInt <$> BS.split ',' contents)
    let inputMap = M.fromListWith (+) $ zip input [1,1..]
    putStrLn "Solution to part 1 is:"
    print $ sum $ iterate update inputMap !! 80
    putStrLn "Solution to part 2 is:"
    print $ sum $ iterate update inputMap !! 256
    
