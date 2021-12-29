import Data.List hiding (nub)

type Vector = (Int, Int, Int)
type Scanner = [Vector]
type Beacons = [Vector]
type ScanPos = Vector

---- Vector operations
transforms :: [Vector]
transforms = [(a,b,c) | a<-[1,-1], b<-[1,-1], c<-[1,-1]]

rotations :: [Vector] -> [[Vector]]
rotations vs = transpose [[(a,b,c), (a,c,b), (b,a,c), (b,c,a), (c,a,b), (c,b,a)] | (a,b,c) <- vs]

vmult :: Vector -> Vector -> Vector
vmult (a,b,c) (a',b',c') = (a*a', b*b', c*c')

vadd :: Vector -> Vector -> Vector
vadd (a,b,c) (a', b', c') = (a+a', b+b', c+c')

vminus :: Vector -> Vector -> Vector
vminus (a,b,c) (a', b', c') = (a-a', b-b', c-c')

---- Scanner operations
appendUniq :: Scanner -> Scanner -> Scanner
appendUniq s1 s2 = nub $ s1 ++ s2

---- all the ways a list of vector can be transformed and rotated
allForms :: Scanner -> [[Vector]]
allForms vs = [[vmult t r | r <- rs] | rs <- rotations vs, t <- transforms]

---- Crosschecking scanners
crossCheck :: Scanner -> Scanner -> (Scanner, ScanPos, Bool)
crossCheck s0 s1 = xCheck (allForms s1)
  where
    xCheck [] = (s0, (0,0,0), False)
    xCheck (vs:vss) | null match = xCheck vss
                    | otherwise  = (appendUniq s0 (relocate vs), key, True)
      where
        crossAdded = [vadd a b | a <- vs, b <- s0]
        match      = filter ((>= 12) . length) $ group $ sort crossAdded
        key        = head $ head match
        relocate   = map (vminus key)


crossCheckAll :: [Scanner] -> ([ScanPos], Beacons)
crossCheckAll (s0:ss') = xCheckAll [] s0 ss'
  where
    xCheckAll sps bs [] = (sps, bs)
    xCheckAll sps bs (s:ss) | success   = xCheckAll (scanpos:sps) (appendUniq bs res) ss
                            | otherwise = xCheckAll sps bs (ss ++ [s])
       where
         (res, scanpos, success) = crossCheck bs s

---- Manhattan distance
manhattan :: Vector -> Vector -> Int
manhattan (a,b,c) (a',b',c') = abs (a-a') + abs (b-b') + abs (c-c')

maxManhattan :: [Vector] -> Int
maxManhattan vs = maximum [manhattan a b | a <- vs, b <- vs]

---- Other
nub :: Ord a => [a] -> [a]
nub = map head . group . sort

---- IO
readVector :: String -> Vector
readVector s = read ('(' : s ++ ")")

readScanner :: [String] -> Scanner
readScanner = map readVector . tail

readScanners :: [String] -> [Scanner]
readScanners [] = []
readScanners lns = readScanner section : readScanners rest
  where
    (section, rest') = break null lns
    rest             = if null rest' then [] else tail rest'

main = do
  ss <- readScanners . lines <$> readFile "input.txt"
  let (scanpositions, beacons) = crossCheckAll ss
  print $ length beacons
  print $ maxManhattan scanpositions
