import Data.List (transpose)
import Text.Regex.TDFA ((=~), getAllTextMatches)

checkLine :: String -> Int
checkLine input = length (getAllTextMatches (input =~ "(M.M.A.S.S)|(S.S.A.M.M)|(S.M.A.S.M)|(M.S.A.M.S)") :: [String])

blocks lines = [drop y $ take (3+y) $ map (drop x . take (3+x)) lines | y <- [0..(length lines - 3)], x <- [0..(length (head lines)-3)]]

main = do
  input <- getContents
  let b = blocks $ lines input
  let c = length $ filter ((==1) . checkLine) $ map concat b
  print c
