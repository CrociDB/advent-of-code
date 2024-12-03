import Text.Regex.TDFA ((=~), getAllTextMatches)

parseMulInput :: String -> [String]
parseMulInput input = getAllTextMatches (input =~ "(mul\\([0-9]+,[0-9]+\\))|(do\\(\\))|(don\'t\\(\\))")

parseNumbers :: String -> [Int]
parseNumbers input = map read $ getAllTextMatches (input =~ "[0-9]+")

getAllEnabled [] instr enabled = instr
getAllEnabled (x:xs) instr enabled = case x of
  "do()" -> getAllEnabled xs instr True
  "don't()" -> getAllEnabled xs instr False
  _ -> getAllEnabled xs (if enabled then instr ++ [x] else instr) enabled

main = do
  input <- getContents
  let m = parseMulInput input
  let s = getAllEnabled m [] True
  print $ sum $ map (product . parseNumbers) s
