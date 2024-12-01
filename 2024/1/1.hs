import Data.List (sort)

separateList (a,b) [x1, x2] = (a ++ [read x1 :: Int], b ++ [read x2 :: Int])

main = do
    input <- getContents
    let (l1, l2) = foldl separateList ([],[]) $ map words $ lines input
    let s = sum $ map (\(x, y) -> abs (x - y)) $ zip (sort l1) (sort l2)
    print s
