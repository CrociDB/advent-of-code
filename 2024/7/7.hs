import Control.Monad (replicateM)
import Data.List.Split (splitOn)

toIntList :: [String] -> [Int]
toIntList = map read

toLineData line = (read (head ln) :: Int, toIntList (filter (not . null) (splitOn " " (ln !! 1))))
  where
    ln = splitOn ":" line

solve (total, xs) = total * min 1 (length $ filter f $ map (applyOps xs) opcombs)
  where
    opcombs = replicateM (length xs - 1) [(+), (*)]
    applyOps xs ops = foldr (\(v,op) a -> a `op` v) 0 $ reverse $ zip xs ((+) : ops)
    f x = x == total

main = do
    input <- getContents
    let l = map toLineData $ lines input
    print $ sum $ map solve l

