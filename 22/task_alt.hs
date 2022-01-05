import Data.List
import Data.Maybe

data BoxType = On | Off deriving (Eq, Show)

data Dimensions = Dimensions { dlx, dhx, dly, dhy, dlz, dhz :: Int }
                | Null
                deriving (Eq)

data Box = World [Box]
         | Box    Dimensions BoxType [Box]
         | ModBox Dimensions BoxType
           deriving (Eq)

instance Show Dimensions where
  show Null = "Null"
  show d = pos ++ "->" ++ dim
    where
      dim = "(" ++ (intercalate "x" . map (show . (+1)))
                    [dhx d - dlx d, dhy d - dly d, dhz d - dlz d] ++ ")"
      pos = "(" ++ (intercalate "," . map show) ([dlx,dly,dlz] <*> [d]) ++ ")"

instance Show Box where
  show (World bs) = "World with " ++ show (length bs) ++ " boxes"
  show (Box d t bs) = "Box " ++ show t ++ " " ++ show d ++
                      " with " ++ show (length bs) ++ " modboxes"
  show (ModBox d t) = "ModBox " ++ show t ++ " " ++ show d

volume :: Box -> Int
volume b = product [dx b, dy b, dz b]

(+:) :: Box -> Box -> Box
(World bs) +: bnew@(Box _ t _) =
  case t of
    On -> World (bnew : map (+: bnew) bs)
    _  -> World (map (+: bnew) bs)
b@(Box d t mbs) +: bnew
  | overlaps b bnew = Box d t (modOverlapOff b bnew : fixMbs mbs)
  | otherwise = b
  where
    fixMbs [] = []
    fixMbs (mb:mbs') | overlaps mb bnew = mb : modOverlapNegate mb bnew : fixMbs mbs'
                    | otherwise        = mb : fixMbs mbs'
_ +: _ = undefined

modOverlapNegate :: Box -> Box -> Box
modOverlapNegate mb@(ModBox _ t) bnew = ModBox (overlap mb bnew) (toggle t)
modOverlapNegate _ _ = undefined

modOverlapOff :: Box -> Box -> Box
modOverlapOff b1 b2 = ModBox (overlap b1 b2) Off

overlap :: Box -> Box -> Dimensions
overlap b1 b2 | b1lx > b2hx || b1hx < b2lx ||
                b1ly > b2hy || b1hy < b2ly ||
                b1lz > b2hz || b1hz < b2lz = Null
              | otherwise = Dimensions
                            (max b1lx b2lx) (min b1hx b2hx)
                            (max b1ly b2ly) (min b1hy b2hy)
                            (max b1lz b2lz) (min b1hz b2hz)
  where
    (b1lx, b1hx, b1ly, b1hy, b1lz, b1hz) =
      (lx b1, hx b1, ly b1, hy b1, lz b1, hz b1)
    (b2lx, b2hx, b2ly, b2hy, b2lz, b2hz) =
      (lx b2, hx b2, ly b2, hy b2, lz b2, hz b2)

overlaps :: Box -> Box -> Bool
overlaps b1 b2 = overlap b1 b2 /= Null

onCount :: Box -> Int
onCount (World bs) = sum $ map onCount bs
onCount b@(Box _ On bs)  = volume b + sum (map onCount bs)
onCount b@(Box _ Off bs) = sum (map onCount bs) - volume b
onCount b@(ModBox _ On)  = volume b
onCount b@(ModBox _ Off) = - volume b

box :: Dimensions -> BoxType -> Box
box d t = Box d t []

toggle :: BoxType -> BoxType
toggle t = if t == On then Off else On

hx,lx,hy,ly,hz,lz :: Box -> Int
lx (World _)   = error ""
lx (Box d _ _)  = dlx d
lx (ModBox d _) = dlx d
hx (World _)   = error ""
hx (Box d _ _)  = dhx d
hx (ModBox d _) = dhx d
ly (World _)   = error ""
ly (Box d _ _)  = dly d
ly (ModBox d _) = dly d
hy (World _)   = error ""
hy (Box d _ _)  = dhy d
hy (ModBox d _) = dhy d
lz (World _)   = error ""
lz (Box d _ _)  = dlz d
lz (ModBox d _) = dlz d
hz (World _)   = error ""
hz (Box d _ _)  = dhz d
hz (ModBox d _) = dhz d

-- box size per dimension "delta x" etc
-- always add 1 because inclusive range
dx, dy, dz :: Box -> Int
dx b = hx b - lx b +1
dy b = hy b - ly b +1
dz b = hz b - lz b +1

newWorld :: Box
newWorld = World []

readBox :: (Int, Int) -> String -> Maybe Box
readBox limit str | rangeOk   = Just $ box dim t
                  | otherwise = Nothing
  where
    rangeOk      = limit == (0,0) || dimInRange limit dim
    dim          = dimFromRange (pr xrange) (pr yrange) (pr zrange)
    t            = if cmd == "on" then On else Off
    (cmd, rst)   = break (==' ') str
    (xrange, r2) = break (==',') $ drop 3 rst
    (yrange, r3) = break (==',') $ drop 3 r2
    (zrange, _)  = break (==',') $ drop 3 r3

dimFromRange :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Dimensions
dimFromRange (a,b) (c,d) (e,f) = Dimensions a b c d e f

dimInRange :: (Int, Int) -> Dimensions -> Bool
dimInRange (l,h) (Dimensions a b c d e f)
  = and [a >= l, c >= l, e >= l, b <= h, d <= h, f <= h]
dimInRange _ _ = undefined

-- parse range
pr :: String -> (Int, Int)
pr s = (read left, read right)
  where
    (left, r1) = break (=='.') s
    right      = drop 2 r1

fr0  = (0,0)
fr50 = (-50,50)

main :: IO ()
main = do
  bxn <- map (readBox fr0) . lines <$> readFile "input.txt"
  let res = foldl (+:) newWorld $ map fromJust $ filter isJust bxn
  --print bxn
  print res
  print $ onCount res
