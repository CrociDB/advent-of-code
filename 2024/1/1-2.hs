import Data.List (sort)

separateList (a,b) [x1, x2] = (a ++ [read x1 :: Int], b ++ [read x2 :: Int])

main = do
    input <- getContents
    let (l1, l2) = foldl separateList ([],[]) $ map words $ lines input
    let s = sum [x * length (filter (== x) l2) | x <- l1]
    print s
