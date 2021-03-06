module Main where

countIncreasses :: [Int] -> Int
countIncreasses [] = 0
countIncreasses [_] = 0
countIncreasses xs = sum $ fromEnum . (> 0) <$> zipWith (flip (-)) xs (tail xs)

slideWindows :: [Int] -> [[Int]]
slideWindows [] = []
slideWindows [_] = []
slideWindows [_,_] = []
slideWindows input@(_:xs) = take 3 input : slideWindows xs

main :: IO ()
main = do
    content <- getContents
    let input = read @Int <$> words content
    putStrLn  "Solution 1 is: "
    print $ countIncreasses input
    putStrLn "Solution 2 is: "
    print $ countIncreasses . fmap sum . slideWindows $ input