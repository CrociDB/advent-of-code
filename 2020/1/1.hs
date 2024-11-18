import Data.List (tails)

crossPairs :: [a] -> [(a, a)]
crossPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

main :: IO ()
main = do
    input <- getContents
    let integers = map read (lines input) :: [Int]

    let r = filter (\(x, y) -> x + y == 2020) $ crossPairs integers
    let (a, b) = head r
    let result = a * b
    print result
