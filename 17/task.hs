
type Velocity = (Int, Int) -- (x, y) growing upward and to the right
type Pos      = (Int, Int) -- (x, y)
type Area     = (Pos, Pos) -- upper left corner, lower right corner
type Probe    = (Pos, Velocity)
type Path     = [Probe]


-- given a certain area, what are the min and max REASONABLE velocities
reasonableVelocityBoundaries :: Area -> (Velocity, Velocity)
reasonableVelocityBoundaries ((alx, _), (arx, ary)) = ((minX, 1), (maxX, abs ary -1))
  where
    nearestX     = if alx > 0 then alx else arx
    (minX, maxX) = if alx > 0 then (mx, nearestX) else (nearestX, mx)
    mx           = signX * head (dropWhile (\x -> trival x < abs nearestX) [1..])
    signX        = if alx > 0 then 1 else (-1)

-- given a certain area, what are ALL min and max possible velocities
possibleVelocityBoundaries :: Area -> (Velocity, Velocity)
possibleVelocityBoundaries ((alx, _), (arx, ary)) = ((minX, ary), (maxX, abs ary -1))
  where
    nearestX     = if alx > 0 then alx else arx
    farthestX    = if alx > 0 then arx else alx
    (minX, maxX) = if alx > 0 then (mx, farthestX) else (farthestX, mx)
    mx           = signX * head (dropWhile (\x -> trival x < abs nearestX) [1..])
    signX        = if alx > 0 then 1 else (-1)


trival :: Int -> Int
trival x = x * (x+1) `div` 2

trySmartVelocities :: Area -> [Velocity]
trySmartVelocities a = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
  where
    ((minX, minY), (maxX, maxY)) = reasonableVelocityBoundaries a

tryAllVelocities :: Area -> [Velocity]
tryAllVelocities a = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
  where
    ((minX, minY), (maxX, maxY)) = possibleVelocityBoundaries a

shoot :: Area -> Velocity -> Path
shoot a v = takeUntil (done a) $ iterate step ((0,0), v)

-- did this shot end up inside target
validShot :: Area -> Path -> Bool
validShot _ [] = False
validShot a p  = insideTarget a $ last p

-- highest y-value on a path
highest :: Path -> Int
highest [] = 0
highest p = maximum $ map (snd . fst) p

step :: Probe -> Probe
step ((px, py), (vx, vy)) = ((px + vx, py + vy), (vx', vy-1))
  where vx' | vx > 0    = vx - 1
            | vx < 0    = vx + 1
            | otherwise = 0

-- inside target or passed target area
done :: Area -> Probe -> Bool
done area@((alx, _), (arx, ary)) probe@((px, py), _)
  = (alx < 0 && px < alx) || -- x outside negative target
    (arx > 0 && px > arx) || -- x outside positive target
    py < ary ||              -- y below target
    insideTarget area probe

insideTarget :: Area -> Probe -> Bool
insideTarget ((alx, aly), (arx, ary)) ((px, py), _)
  = px >= alx && px <= arx && py <= aly && py >= ary

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) | p x       = [x]
                   | otherwise = x : takeUntil p xs

main :: IO ()
main = do
  let a = ((20,-5), (30,-10))
  let b = ((230,-57), (283,-107))
  print $ maximum $ map highest $ filter (validShot a) $ map (shoot a) $ trySmartVelocities a
  print $ maximum $ map highest $ filter (validShot b) $ map (shoot b) $ trySmartVelocities b
  putStrLn "-------"
  print $ length $ filter (validShot a) $ map (shoot a) $ tryAllVelocities a
  print $ length $ filter (validShot b) $ map (shoot b) $ tryAllVelocities b
