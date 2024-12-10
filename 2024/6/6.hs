import Data.List (elemIndex)
import Data.Maybe (fromJust)
data Path = Incomplete [String] | Complete deriving (Show, Eq)

dirchars = "^>v<"

replace :: Char -> Char -> String -> String
replace a b = map $ \c -> if c == a then b else c

removePositions :: String -> String
removePositions [] = []
removePositions (c:cs) = if c `elem` dirchars then '.' : removePositions cs else c : removePositions cs

nextDirection :: Char -> Char
nextDirection c = dirchars !! (((fromJust (c `elemIndex` dirchars)) + 1) `mod` length dirchars)

nextPosition :: (Int,Int) -> Char -> (Int,Int)
nextPosition (x,y) '^' = (x,y-1)
nextPosition (x,y) '>' = (x+1,y)
nextPosition (x,y) 'v' = (x,y+1)
nextPosition (x,y) '<' = (x-1,y)

addpos positions pos = if pos `elem` positions then positions else positions ++ [pos]

getcharat board (x,y) = (board !! y) !! x

findposline line = filter (\(c, n) -> c `elem` dirchars) $ zip line [0..length line]
findposdir board = (pos, getcharat board pos)
  where
    res = head $ filter (not . null . fst) $ zipWith (\l n -> (findposline l, n)) board [0..length board]
    pos = (snd (head (fst res)), snd res)

withinBoundaries board (x,y) = x >= 0 && y >= 0 && x < length (head board) && y < length board

moveTo :: [String] -> ((Int,Int), Char) -> [String]
moveTo board ((x,y), dir) = take y cleanboard ++ [newline] ++ drop (y + 1) cleanboard
  where
    cleanboard = map removePositions board
    line = cleanboard !! y
    newline = take x line ++ [dir] ++ drop (x + 1) line

move Complete = Complete
move (Incomplete board) = if withinBoundaries board nextpos then next else Complete
  where
    (pos, dir) = findposdir board
    nextpos = nextPosition pos dir
    charnext = getcharat board nextpos
    next = if charnext == '#' then Incomplete (moveTo board (pos, nextDirection dir)) else Incomplete (moveTo board (nextpos, dir))

moveAll :: Path -> [(Int, Int)] -> [(Int, Int)]
moveAll Complete positions = positions
moveAll (Incomplete board) positions = moveAll (move (Incomplete board)) $ addpos positions pos
  where
    (pos, _) = findposdir board

printBoard (Incomplete []) = do return ()
printBoard (Incomplete (line:lines)) = do
  print line
  printBoard (Incomplete lines)
  

main = do
    input <- getContents
    let board = lines input
    -- print board
    -- printBoard $ move (Incomplete board)
    print $ length $ moveAll (Incomplete board) []
