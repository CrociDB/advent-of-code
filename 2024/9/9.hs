import Control.Monad (replicateM)
import Data.Char (isDigit, digitToInt)

data Value where
  Data :: String -> Value
  deriving Show

genIdBlock n = concat $ zipWith (\i b -> [Data (show i), Data b]) [0..(n `div` 2)] (head (replicateM (n `div` 2) ["."]))

generateBlocks dense = concat $ foldr build [] $ reverse $ zip d (genIdBlock (length d))
  where
    d = dense ++ [0]
    build (n, Data s) acc = acc ++ replicateM n s

removeTrailingSpace xs = reverse $ dropWhile (=='.') $ reverse xs

defrag before [] = before
defrag before (x:xs) = if x == '.' then defrag (before ++ [last txs]) (init txs) else defrag (before ++ [x]) xs
  where
    txs = removeTrailingSpace xs

main = do
    input <- getContents
    let disk = generateBlocks $ map digitToInt (filter isDigit input)
    let d = defrag [] disk
    print $ foldr (\(a,b) acc -> acc + (a * b)) 0 $ zip (map digitToInt d) [0..length d]


