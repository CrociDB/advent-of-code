import Data.List (sort)

toIntList :: [String] -> [[Int]]
toIntList = map (map read . words)

removeByIndex :: Int -> [a] -> [a]
removeByIndex _ [] = []
removeByIndex i xs = take i xs ++ drop (i+1) xs

safe xs = length (filter f diff) == length diff && abs (sum diff) == sum (map abs diff)
  where
    diff = zipWith (-) xs $ drop 1 xs
    f x = abs x > 0 && abs x <= 3

isSafe xs = safe xs || any safe removeOne
  where
    removeOne = foldl (\acc (x, n) -> acc ++ [removeByIndex n xs]) [] $ zip xs [0..length xs]

main = do
    input <- getContents
    let n = toIntList $ lines input
    print $ length $ filter isSafe n
