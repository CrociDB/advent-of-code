import Data.List (tails)

crossThruple :: [a] -> [(a, a, a)]
crossThruple xs = [(x, y, z) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs]

main :: IO ()
main = do
    input <- getContents
    let integers = map read (lines input) :: [Int]

    let r = filter (\(x, y, z) -> x + y + z == 2020) $ crossThruple integers
    let (a, b, c) = head r
    let result = a * b * c
    print result
