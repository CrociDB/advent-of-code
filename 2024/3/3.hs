import Text.Regex.TDFA ((=~), getAllTextMatches)

parseMulInput :: String -> [String]
parseMulInput input = getAllTextMatches (input =~ "mul\\([0-9]+,[0-9]+\\)")

parseNumbers :: String -> [Int]
parseNumbers input = map read $ getAllTextMatches (input =~ "[0-9]+")

main = do
  input <- getContents
  let m = map parseNumbers $ parseMulInput input
  print $ sum $ map (foldl (*) 1) m
