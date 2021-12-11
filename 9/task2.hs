-- This idea worked but it kinda grew out of control
-- and it doesn't look pretty

import Data.List
import Data.Char (digitToInt)
import Data.Ord

data Location = Wall | Floor Basin deriving Eq
type Basin = Char
newtype OceanFloor = OceanFloor { locations :: [[Location]] }
data Cursor = Cursor { cx :: Int,
                       cy :: Int,
                       ofloor :: OceanFloor }

instance Show Location where
  show Wall = " "
  show (Floor b) = [b]

instance Show OceanFloor where
  show (OceanFloor rows) = intercalate "\n" . map (concatMap show) $ rows

instance Show Cursor where
  show (Cursor x y f) = "{ x=" ++ show x ++ ", y=" ++ show y ++ " }\n" ++ show f ++ "\n"

noBasin,trailBasin :: Char
noBasin    = '@'
trailBasin = '-'

newCursor :: OceanFloor -> Cursor
newCursor = Cursor 0 0

getBasin :: Location -> Basin
getBasin Wall = error "No basin in a Wall"
getBasin (Floor b) = b

above,below,leftof,rightof :: Cursor -> Cursor
above   (Cursor x y f) = Cursor x (y - 1) f
below   (Cursor x y f) = Cursor x (y + 1) f
leftof  (Cursor x y f) = Cursor (x - 1) y f
rightof (Cursor x y f) = Cursor (x + 1) y f

marked,unmarked :: Location -> Bool
marked Wall      = True
marked (Floor c) = c /= noBasin && c /= trailBasin
unmarked = not . marked

allMarked :: Cursor -> Bool
allMarked (Cursor _ _ f) = (not . any unmarked . concat . locations ) f

type Move = Cursor -> Cursor
moves :: [(Move, Move)]
moves = [(rightof, leftof), (below, above), (leftof, rightof), (above, below)]

markBasins :: Basin -> Cursor -> Cursor
markBasins b c = case nextUnmarked $ goto (0, 0) c of
                   Nothing   -> c
                   Just nxt -> markBasins nxtb $ markBasin nxtb Nothing nxt
  where
    nxtb  = succ b

nextUnmarked :: Cursor -> Maybe Cursor
nextUnmarked c@(Cursor _ _ f)
  | allMarked c = Nothing
  | otherwise = Just (goto (x, y) c)
  where
    locs = concat $ locations f
    nummarked = length $ takeWhile marked locs
    x = nummarked `rem` floorWidth f
    y = nummarked `div` floorWidth f

markBasin :: Basin -> Maybe Move -> Cursor -> Cursor
markBasin b bk c
  | marked (at c) = goBack c
  | otherwise = goBack $ markLocation b $
                foldl (\c' (go, from) ->
                          if at (go c') /= Floor trailBasin
                          then markBasin b (Just from) (go c')
                          else c') (markLocation trailBasin c) moves
  where
    goBack c' = case bk of
                  Nothing -> c'
                  Just f -> f c'

markLocation :: Basin -> Cursor -> Cursor
markLocation b c@(Cursor x y f)
  | at c == Wall = c
  | otherwise    = Cursor x y mrkd
  where
    mrkd = OceanFloor $ take y rs ++
           [take x (rs !! y) ++ [Floor b] ++ drop (x+1) (rs !! y)] ++
           drop (y+1) rs
    rs = locations f

countBasins :: Cursor -> [Int]
countBasins (Cursor _ _ f) = (sortOn Down . map length . group . sort . map getBasin .
                              filter (/= Wall) . concat . locations) f

goto :: (Int, Int) -> Cursor -> Cursor
goto (x, y) c = c {cx = x, cy = y}

at :: Cursor -> Location
at (Cursor x y f) | outside x 0 (floorWidth f - 1)  = Wall
                  | outside y 0 (floorHeight f - 1) = Wall
                  | otherwise = (locations f !! y) !! x
  where
    outside :: Ord a => a -> a -> a -> Bool
    outside v l r = v < l || v > r

floorWidth,floorHeight :: OceanFloor -> Int
floorWidth  = length . head . locations
floorHeight = length . locations

readFloor :: [String] -> OceanFloor
readFloor = OceanFloor . map rr
  where
    rr = map (rl . digitToInt)
    rl x | x == 9    = Wall
         | otherwise = Floor noBasin

main :: IO ()
main = do
  rows <- lines <$> readFile "input.txt"
  -- print $ markBasins noBasin $ newCursor (readFloor rows)
  print $ product $ take 3 $ countBasins $ markBasins noBasin $ newCursor (readFloor rows)
