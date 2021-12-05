import Data.List
import Data.Char (digitToInt)
import Data.Ord (comparing)

data Pos = Pos {x, y :: Int}
  deriving Show

instance Eq Pos where
  p1 == p2 = x p1 == x p2 && y p1 == y p2

instance Ord Pos where
  compare = comparing x <> comparing y

main2 :: IO ()
main2 = do
  input <- readCoords "input.txt"
  print $ countBadCoords (>= 2) coordsToAnyLine input

main1 :: IO ()
main1 = do
  input <- readCoords "input.txt"
  print $ countBadCoords (>= 2) coordsToHVLine input

readCoords :: String -> IO [[Pos]]
readCoords filename = map (map readPos . filter (=~ ',') . words) . lines
                      <$> readFile filename

countBadCoords :: (Int -> Bool) -> (Pos -> Pos -> [Pos]) -> [[Pos]] -> Int
countBadCoords flt c2l = length . filter flt . map length .
                         group . sort . concatMap (\ps -> c2l (head ps) (last ps))

-- all positions between two endpoints inclusive, also diagonals (at 45 degree angle)
coordsToAnyLine :: Pos -> Pos -> [Pos]
coordsToAnyLine p1@(Pos x1 y1) p2@(Pos x2 y2)
  | null l    = [Pos x y | (x, y) <- zip (range x1 x2) (range y1 y2)]
  | otherwise = l
  where l = coordsToHVLine p1 p2

-- all positions between two endpoints inclusive
-- HV = only if horiz / vert aligned, no diagonals
coordsToHVLine :: Pos -> Pos -> [Pos]
coordsToHVLine (Pos x1 y1) (Pos x2 y2)
  | x1 == x2 = map      (Pos x1) (range y1 y2)
  | y1 == y2 = map (flip Pos y1) (range x1 x2)
  | otherwise = []

range :: Int -> Int -> [Int]
range a b | a > b     = [a,(a-1)..b]
          | otherwise = [a..b]

readPos :: String -> Pos
readPos = getPos . splitBy ','
  where getPos [x,y] = Pos (read x) (read y)

(=~) :: String -> Char -> Bool
s =~ c = (not . null) $ filter (== c) s

splitBy delim = foldr f [[]]
  where f c a@(x:xs) | c == delim = []:a
                     | otherwise = (c:x):xs
