import Data.List

data Prev = Prev Int | None

newtype Point = Point (Int, [Int])
  deriving Show

neighbours :: Point -> [Int]
neighbours (Point (_, xs)) = xs
value      :: Point -> Int
value      (Point (p, _))  = p


main1 :: IO ()
main1 = do
  rows <- map (map read) . map (map (: [])) . lines <$> readFile "input.txt" :: IO [[Int]]
  let point_rows = map (map (\x -> Point (x, []))) rows
  let mapped = mapAdjPoints point_rows
  let low_points = lowPoints mapped
  print $ scoreLowPoints low_points

scoreLowPoints :: [Point] -> Int
scoreLowPoints = sum . map ((+1) . value)

lowPoints :: [[Point]] -> [Point]
lowPoints = filter isLowPoint . concat

isLowPoint :: Point -> Bool
isLowPoint (Point (v, vs)) = v < minimum vs

mapAdjPoints :: [[Point]] -> [[Point]]
mapAdjPoints = transpose . map (adjRow None) . transpose . map (adjRow None)

adjRow :: Prev -> [Point] -> [Point]
adjRow None [x] = error $ show x
adjRow None (p:pr:ps)     = addNeighbours p [value pr]    : adjRow (Prev (value p)) (pr:ps)
adjRow (Prev l) (p:pr:ps) = addNeighbours p [l, value pr] : adjRow (Prev (value p)) (pr:ps)
adjRow (Prev l) [p]       = [addNeighbours p [l]]

addNeighbours :: Point -> [Int] -> Point
addNeighbours (Point (p, ns)) ns' = Point (p, ns' ++ ns)
