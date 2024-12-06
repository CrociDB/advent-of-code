import Data.List (transpose)
import Text.Regex.TDFA ((=~), getAllTextMatches)

checkLine :: String -> Int
checkLine input = length (getAllTextMatches (input =~ "XMAS") :: [String])

diagonalsTLBR :: [[a]] -> [[a]]
diagonalsTLBR [] = []
diagonalsTLBR matrix = [ [ matrix !! (i - k) !! k | k <- [0..i], k <= n, (i - k) <= m ] | i <- [0..(m + n)] ]
  where
    m = length matrix - 1
    n = length (head matrix) - 1

diagonalsTRBL :: [[a]] -> [[a]]
diagonalsTRBL [] = []
diagonalsTRBL matrix = [ [ matrix !! (i - k) !! (n - k) | k <- [0..i], k <= n, (i - k) <= m ] | i <- [0..(m + n)] ]
  where
    m = length matrix - 1
    n = length (head matrix) - 1

getAllLines input = total ++ map reverse total
  where
    total = input ++ vertical ++ diagonall ++ diagonalr
    vertical = transpose input
    diagonall = filter (\s -> length s > 3) $ diagonalsTLBR input
    diagonalr = filter (\s -> length s > 3) $ diagonalsTRBL input

main = do
  input <- getContents
  let result = sum $ map checkLine $ getAllLines $ lines input
  print result 
