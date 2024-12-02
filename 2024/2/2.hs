import Data.List (sort)

toIntList :: [String] -> [[Int]]
toIntList = map (map read . words)

isSafe xs = length (filter f diff) == length diff && abs (sum diff) == sum (map abs diff)
  where
    diff = zipWith (-) xs $ drop 1 xs
    f x = abs x > 0 && abs x <= 3

main = do
    input <- getContents
    let n = toIntList $ lines input
    print $ length $ filter isSafe n
