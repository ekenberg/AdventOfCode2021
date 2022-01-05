import Data.Maybe

data Dimensions = Nowhere | Dim { x1, x2, y1, y2, z1, z2 :: Int } deriving Eq
data CellType   = On | Off deriving Eq
newtype Reactor = Reactor [Box]
data Box        = Box    Dimensions CellType [ModBox]
data ModBox     = ModBox Dimensions CellType

class Shape a where
  dimensions :: a -> Dimensions
  celltype   :: a -> CellType
  volume     :: a -> Int
  volume s = product [x2 d - x1 d +1, y2 d - y1 d +1, z2 d - z1 d +1]
    where d = dimensions s

instance Shape Box    where
  dimensions (Box d _ _)  = d
  celltype   (Box _ t _)  = t

instance Shape ModBox where
  dimensions (ModBox d _) = d
  celltype   (ModBox _ t) = t

addToReactor :: Reactor -> Box -> Reactor
addToReactor (Reactor bs) bn@(Box _ t _)
  | t == On   = Reactor (bn : map boxAdd bs)
  | otherwise = Reactor (map boxAdd bs)
  where
    boxAdd b@(Box d t' mbs)
      | overlaps bn b = Box d t' (ModBox (overlap bn b) Off : offsetMods mbs)
      | otherwise     = b
    offsetMods [] = []
    offsetMods (mb:mbs') | overlaps bn mb = mb : mb' : offsetMods mbs'
                         | otherwise      = mb : offsetMods mbs'
      where mb' = ModBox (overlap bn mb) (if celltype mb == On then Off else On)

overlap :: (Shape a, Shape b) => a -> b -> Dimensions
overlap b1 b2
  | ox1 <= ox2 && oy1 <= oy2 && oz1 <= oz2 = Dim ox1 ox2 oy1 oy2 oz1 oz2
  | otherwise                              = Nowhere
  where
    (d1, d2)   = (dimensions b1, dimensions b2)
    (ox1, ox2) = (max (x1 d1) (x1 d2), min (x2 d1) (x2 d2))
    (oy1, oy2) = (max (y1 d1) (y1 d2), min (y2 d1) (y2 d2))
    (oz1, oz2) = (max (z1 d1) (z1 d2), min (z2 d1) (z2 d2))

overlaps :: (Shape a, Shape b) => a -> b -> Bool
overlaps b1 b2 = overlap b1 b2 /= Nowhere

count :: CellType -> Reactor -> Int
count t (Reactor bs) = sum $ map bcount bs
  where
    bcount b@(Box _ t' mbs)
      = sum (map mbcount mbs) + if t == t' then volume b else 0
    mbcount mb@(ModBox _ t') = volume mb * if t == t' then 1 else -1

newReactor :: Reactor
newReactor = Reactor []

readBox :: (Int, Int) -> String -> Maybe Box
readBox limit str | rangeOk   = Just $ Box dim t []
                  | otherwise = Nothing
  where
    rangeOk      = limit == (0,0) || dimInRange limit dim
    dim          = Dim (read xr1) (read xr2) (read yr1)
                   (read yr2) (read zr1) (read zr2)
    t            = if cmd == "on" then On else Off
    (cmd, rst)  = break (==' ') str
    (xr1, rst2) = break (=='.') $ drop 3 rst
    (xr2, rst3) = break (==',') $ drop 2 rst2
    (yr1, rst4) = break (=='.') $ drop 3 rst3
    (yr2, rst5) = break (==',') $ drop 2 rst4
    (zr1, rst6) = break (=='.') $ drop 3 rst5
    (zr2, _)    = break (==',') $ drop 2 rst6

dimInRange :: (Int, Int) -> Dimensions -> Bool
dimInRange (l,h) (Dim a b c d e f)
  = and [a >= l, c >= l, e >= l, b <= h, d <= h, f <= h]
dimInRange _ _ = undefined

fr0  = (0,0)
fr50 = (-50,50)

main :: IO ()
main = do
  bxn1 <- map (readBox fr50) . lines <$> readFile "test_input1.txt"
  bxn2 <- map (readBox fr50) . lines <$> readFile "test_input.txt"
  bxn3 <- map (readBox fr0)  . lines <$> readFile "test_input2.txt"
  let res1 = foldl addToReactor newReactor $ map fromJust $ filter isJust bxn1
  let res2 = foldl addToReactor newReactor $ map fromJust $ filter isJust bxn2
  let res3 = foldl addToReactor newReactor $ map fromJust $ filter isJust bxn3
  putStrLn "-- TESTS"
  print $ count On res1
  print $ count On res2
  print $ count On res3

  putStrLn "-- REAL"
  bxn_1 <- map (readBox fr50) . lines <$> readFile "input.txt"
  bxn_2 <- map (readBox fr0)  . lines <$> readFile "input.txt"
  let res_1 = foldl addToReactor newReactor $ map fromJust $ filter isJust bxn_1
  let res_2 = foldl addToReactor newReactor $ map fromJust $ filter isJust bxn_2
  print $ count On res_1
  print $ count On res_2
