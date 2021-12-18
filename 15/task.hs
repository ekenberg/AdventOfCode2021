
import AStar
import Data.Char
import Data.Maybe

type Matrix = [[Int]]
type Pos    = (Int, Int)

testMatrix :: Matrix
testMatrix = [[1,2,3],
              [2,3,4],
              [7,8,9]]

mcosts :: Matrix -> Pos -> [(Pos, Int)]
mcosts mx (r,c) = costmap adjacent
  where
    costmap  = map (\p -> (p, at mx p))
    h        = length mx
    w        = length $ head mx
    adjacent = filter
      (\(r',c') -> r' >= 0 && c' >= 0 && r' < h && c' < w)
      [(r-1,c), (r+1,c), (r, c-1), (r, c+1)]

at :: Matrix -> Pos -> Int
at mx (r,c) = (mx !! r) !! c

spreadMx :: Int -> Matrix -> Matrix
spreadMx n mx = concat $ take n $ iterate plusMx $ hStitch n mx

hStitch :: Int -> Matrix -> Matrix
hStitch n = map (concat . take n . iterate plusRow)

plusMx :: Matrix -> Matrix
plusMx = map plusRow

plusRow :: [Int] -> [Int]
plusRow = map ((+1) . (`rem` 9))

readMatrix :: String -> IO Matrix
readMatrix fn = map (map ((read :: String -> Int) . (: []))) . lines <$> readFile fn

showMatrix :: Matrix -> IO ()
showMatrix = putStr . unlines . map (map intToDigit)

main2 = do
  mx <- spreadMx 5 <$> readMatrix "input.txt"
  let (rows, cols) = (length mx -1, length (head mx) -1)
  print $ fst $ fromJust $ astarSearch (0,0) (== (rows,cols)) (mcosts mx) (const 0)

main = do
  mx <- readMatrix "input.txt"
  let (rows, cols) = (length mx -1, length (head mx) -1)
  print $ fst $ fromJust $ astarSearch (0,0) (== (rows,cols)) (mcosts mx) (const 0)
