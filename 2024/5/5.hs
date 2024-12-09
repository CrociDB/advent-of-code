import Data.List.Split (splitOn)

toIntList :: [String] -> [Int]
toIntList = map read

splitInput (x : xs) r = if x == "" then (r, xs) else splitInput xs (r ++ [x])

member xs x = x `elem` xs

validate rules before (x : xs) = validate_before && validate_after && next
  where
    left = map (\[a, b] -> a) $ filter (\[a, b] -> b == x) rules -- numbers to the left of X
    right = map (\[a, b] -> b) $ filter (\[a, b] -> a == x) rules -- numbers to the right of X
    validate_before = not $ any (member right) before
    validate_after = not $ any (member left) xs
    next = null xs || validate rules (before ++ [x]) xs

midlist xs = xs !! (ceiling (fromIntegral (length xs) / 2) - 1)

main = do
    input <- getContents
    let i = lines input
    let (r, p) = splitInput i []
    let rules = map (toIntList . splitOn "|") r
    let pages = map (toIntList . splitOn ",") p
    let validated = map (validate rules []) pages
    let s = sum $ zipWith (\xs v -> if v then midlist xs else 0) pages validated
    print s
