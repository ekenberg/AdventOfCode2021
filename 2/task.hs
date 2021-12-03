
main2 :: IO ()
main2 = do
  input <- readFile "input.txt"
  let (a, h, d) = navigateWithAim $ lines input
  print $ h*d

aimstep :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
aimstep ("forward", steps) (aim, hpos, depth) = (aim, hpos + steps, depth + aim * steps)
aimstep ("down", steps) (aim, hpos, depth)    = (aim + steps, hpos, depth)
aimstep ("up", steps) (aim, hpos, depth)      = (aim - steps, hpos, depth)
aimstep _ _ = undefined

navigateWithAim :: [String] -> (Int, Int, Int)
navigateWithAim = foldl (flip aimstep) (0, 0, 0) . parseDirections

---------------------------------------------------------------

parseDirections :: [String] -> [(String, Int)]
parseDirections = map ((\x -> (head x, (read :: String -> Int) (last x))) . words)

---------------------------------------------------------------
main1 :: IO ()
main1 = do
  input <- readFile "input.txt"
  let (h, d) = navigate $ lines input
  print $ h*d

navigate :: [String] -> (Int, Int)
navigate = foldr navstep (0, 0) . parseDirections

navstep :: (String, Int) -> (Int, Int) -> (Int, Int)
navstep ("forward", steps)  (hpos, depth) = (hpos + steps, depth)
navstep ("down", steps)     (hpos, depth) = (hpos, depth + steps)
navstep ("up", steps)       (hpos, depth) = (hpos, depth - steps)
navstep _ _ = error "wrong input"
