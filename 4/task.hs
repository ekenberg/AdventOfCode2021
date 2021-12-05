import Data.List

type Board  = [Row]
type Row    = [Square]
data Square = U Int | M Int
  deriving Show

main2 :: IO ()
main2 = do
  indata <- filter ((0 <) . length) . lines <$> readFile "input.txt"
  let draws  = map read $ splitBy ',' $ head indata
  let boards = readBoards $ tail indata
  let (losing_draw, losing_board) = findLoser draws boards
  print $ losing_draw * computeScore losing_board

main1 :: IO ()
main1 = do
  indata <- filter ((0 <) . length) . lines <$> readFile "input.txt"
  let draws  = map read $ splitBy ',' $ head indata
  let boards = readBoards $ tail indata
  let (winning_draw, winning_board) = findWinner draws boards
  print $ winning_draw * computeScore winning_board

findLoser :: [Int] -> [Board] -> (Int, Board)
findLoser [] _ = error "No loser: Ran out of draws"
findLoser _ [] = error "No loser: Ran out of boards"
findLoser (d:ds) bs
  | length bs == 1 && hasBingo (head markedBoards) = (d, head markedBoards)
  | otherwise                = findLoser ds losingBoards
  where
    losingBoards = filter (not . hasBingo) markedBoards
    markedBoards = map (markBoard d) bs

findWinner :: [Int] -> [Board] -> (Int, Board)
findWinner [] _ = error "No winner: Ran out of draws"
findWinner _ [] = error "No winner: Ran out of boards"
findWinner (d:ds) bs
  | hasWinner markedBoards = (d, head (winner markedBoards))
  | otherwise              = findWinner ds markedBoards
  where
    markedBoards = map (markBoard d) bs
    winner       = filter hasBingo
    hasWinner    = not . null . winner

markBoard :: Int -> Board -> Board
markBoard d = map (markRow d)

markRow :: Int -> Row -> Row
markRow d = map (markSquare d)

markSquare :: Int -> Square -> Square
markSquare d s@(U m) | d == m = M m
markSquare _ s = s

hasBingo :: Board -> Bool
hasBingo [] = True
hasBingo rows = any fullRow (rows ++ transpose rows)

fullRow :: Row -> Bool
fullRow []       = True
fullRow (M _:rs) = fullRow rs
fullRow _        = False

computeScore :: Board -> Int
computeScore = sum . map sumrow

sumrow :: Row -> Int
sumrow []        = 0
sumrow (U m: ss) = m + sumrow ss
sumrow (_:ss)    = sumrow ss

splitBy delim = foldr f [[]]
  where f c a@(x:xs) | c == delim = []:a
                     | otherwise = (c:x):xs

readBoards :: [String] -> [Board]
readBoards [] = []
readBoards xss | length xss `rem` 5 /= 0
  = error "readBoards need lines in multiples of 5!"
readBoards xss = readBoard (take 5 xss) : readBoards (drop 5 xss)

readBoard :: [String] -> Board
readBoard = map readRow

readRow :: String -> Row
readRow = map (U . read) . words
