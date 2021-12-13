import Data.Char
import Data.List
import Data.Maybe

type Level   = Int        -- 0-9 (or higher)
type Pos     = (Int, Int) -- row, col
data State   = Charging Level | Depleted deriving Show
newtype Cell = Cell State
data Grid    = Grid { cells :: [[Cell]],
                      flashCount :: Int }
type PCell   = (Pos, Cell)

instance Show Cell where
  show (Cell (Charging v)) | v > 9 = "+"
                           | otherwise = [intToDigit v]
  show _                   = " " -- depleted

gmap :: (Cell -> Cell) -> Grid -> Grid
gmap f (Grid cs c) = Grid (map (map f) cs) c

value :: Cell -> Maybe Int
value (Cell (Charging v)) = Just v
value _ = Nothing

ngroup :: Int -> [a] -> [[a]]
ngroup _ [] = []
ngroup n xs = take n xs : ngroup n (drop n xs)

unpack :: [[Cell]] -> [PCell]
unpack cs = [(p, c) | (p, c) <-
                zip [(row, col) | row <- [0..], col <- [0..w-1]] (concat cs) ]
  where w = length $ head cs

pack :: Int -> [PCell] -> [[Cell]]
pack n = ngroup n . map snd

adjacent :: Pos -> [Pos]
adjacent (row, col) =
  [(r, c) | r <- [row-1..row+1], c <- [col-1..col+1], (r,c) /= (row,col)]

chargeAdjacent :: Pos -> [PCell] -> [PCell]
chargeAdjacent pos = map condCharge
  where
    condCharge (p, c) | p `elem` (adjacent pos) = (p, charge c)
                      | otherwise = (p, c)

dup :: a -> (a, a)
dup x = (x, x)

flash :: Grid -> Grid
flash (Grid cs c) = Grid (pack w $ uncurry rFlash $ dup $ unpack cs) c
  where
    w = length $ head cs
    rFlash :: [PCell] -> [PCell] -> [PCell]
    rFlash last [] = last
    rFlash last ((_, (Cell Depleted)):xs) = rFlash last xs
    rFlash last ((p, (Cell (Charging v))):xs)
      | v > 9     = uncurry rFlash $ dup $ chargeAdjacent p $ map (condCharge p) last
      | otherwise = rFlash last xs
    condCharge p (p',c) | p' == p   = (p, deplete c)
                        | otherwise = (p', c)

step :: Grid -> Grid
step = resetDepleted . flash . gmap charge

resetDepleted :: Grid -> Grid
resetDepleted = gmap reset . flashcount
  where
    reset c | isDepleted c = Cell (Charging 0)
            | otherwise    = c
    flashcount (Grid cs cnt) =
      Grid cs (cnt + (length $ concatMap (filter isDepleted) cs))

deplete :: Cell -> Cell
deplete (Cell _) = Cell Depleted

isDepleted :: Cell -> Bool
isDepleted (Cell Depleted) = True
isDepleted _               = False

charge :: Cell -> Cell
charge (Cell (Charging l)) = Cell (Charging (l+1))
charge c                   = c -- depleted

loadLine :: String -> [Cell]
loadLine = map (Cell . Charging . digitToInt)

printGrid :: Grid -> IO ()
printGrid (Grid cs n) = putStr $ unlines $ flashes : map (concatMap show) cs
  where flashes = "Flashes seen: " ++ show n

nApply :: Int -> (a -> a) -> a -> a
nApply 0 _ xs = xs
nApply i f xs = nApply (i-1) f $ f xs

countApplyUntil :: (a -> a) -> (a -> Bool) -> a -> Int
countApplyUntil = cau 0
  where
    cau n f cond x | cond x    = n
                   | otherwise = cau (n+1) f cond (f x)

main2 :: IO ()
main2 = do
  cls <- map loadLine . lines <$> readFile "input.txt"
  let grid = Grid cls 0
  let count = countApplyUntil step
              (all (==0) . concatMap (map (fromJust . value)) . cells) grid
  print count

main1 :: IO ()
main1 = do
  cls <- map loadLine . lines <$> readFile "input.txt"
  let grid = Grid cls 0
  printGrid grid
  putStrLn ""
  printGrid  $ nApply 100 step $ grid
