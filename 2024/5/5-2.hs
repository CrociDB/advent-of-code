import Data.List.Split (splitOn)
import Data.List (permutations, elemIndex)
import Data.Maybe (fromJust)

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

switchPosInList v x xs = take pos xs ++ [v] ++ drop (pos + 1) xs
  where
    pos = fromJust $ elemIndex x xs

makeItValidStep rules before (x : xs) = if validate rules before (x:xs) then before ++ [x] ++ xs else res
  where
    left = map (\[a, b] -> a) $ filter (\[a, b] -> b == x) rules -- numbers to the left of X
    right = map (\[a, b] -> b) $ filter (\[a, b] -> a == x) rules -- numbers to the right of X
    should_be_before = filter (member left) xs
    new_x = head should_be_before
    new_xs = switchPosInList x new_x xs
    res = if null should_be_before then makeItValidStep rules (before ++ [x]) xs else makeItValidStep rules before (new_x:new_xs)

main = do
    input <- getContents
    let i = lines input
    let (r, p) = splitInput i []
    let rules = map (toIntList . splitOn "|") r
    let pages = map (toIntList . splitOn ",") p
    let invalid = filter (not . validate rules []) pages
    let r = map (makeItValidStep rules []) invalid
    let s = sum $ map midlist r
    print s
