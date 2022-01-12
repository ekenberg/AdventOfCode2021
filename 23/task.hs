import Data.List

pos :: Node -> Pos
pos (Step p _)   = p
pos (Cave p _ _) = p

visitor :: Node -> Amphipod
visitor (Step _ a) = a
visitor (Cave _ _ as) | emptyCave = Nobody
                      | otherwise = head $ filter (/= Nobody) as
  where emptyCave = all (== Nobody) as

-- rätt och bra eller feltänkt med datatyp?
visitors :: Node -> [Amphipod]
visitors (Cave _ _ as) = as
visitors (Step _ a)    = [a]

caves :: Scene -> [Node]
caves = filter isCave . path

isCave :: Node -> Bool
isCave Cave {} = True
isCave _       = False

caveFor :: Scene -> Amphipod -> Node
caveFor s a = head $ filter (\n -> isCave n && caveType n == podtype a) (path s)

caveType :: Node -> Podtype
caveType (Cave _ t _) = t
caveType _            = error "not a cave"

caveDone :: Node -> Bool
caveDone (Cave _ t as) = none (== Nobody) as && all ((== t) . podtype) as
caveDone _             = error "not a cave"

cavePop :: Node -> Node
cavePop (Cave i t as) = Cave i t $ replaceFirstP (/= Nobody) Nobody as
cavePop _             = error "not a cave"

cavePush :: Amphipod -> Node -> Node
cavePush a (Cave i t as) = Cave i t $ replaceLastP (== Nobody) a as
cavePush _ _             = error "not a cave"

-----------------------
cavePopCost :: Node -> Cost
cavePopCost = (+1) . cavePushCost

cavePushCost :: Node -> Cost
cavePushCost (Cave _ _ as) = length $ filter (== Nobody) as
cavePushCost _             = error "not a cave"

caveReceives :: Node -> Amphipod -> Bool
caveReceives c@(Cave _ ct as) a = ct == podtype a &&
                                  all (\v-> v == Nobody || podtype v == ct) (visitors c)
caveReceives _ _                = error "not a cave"

freePath :: Scene -> Node -> Node -> Bool
freePath sc fn tn = all checknode range
  where
    range | pos fn < pos tn = filter (\n->pos n > pos fn && pos n <= pos tn) (path sc)
          | otherwise       = filter (\n->pos n < pos fn && pos n >= pos tn) (path sc)
    checknode n | fn == n   = False -- starting position
                | isCave n  = True
                | otherwise = visitor n == Nobody

validMoves :: Scene -> [Move]
validMoves s = sort $ concat $ filter (not . null) $ map findMovesFor $ filter canMove (path s)
  where
    costCalc fn tn = weight (visitor fn) * (startCost fn + pathCost fn tn + endCost tn)
    pathCost fn tn = abs (pos fn - pos tn)
    startCost fn = if isCave fn then cavePopCost fn else 0
    endCost   tn = if isCave tn then cavePushCost tn else 0
    canMove n | visitor n == Nobody = False -- nobody there
              | isCave n && caveReceives n (visitor n) = False -- in home cave
              | otherwise = True
    findMovesFor fn
      | caveReceives mycave (visitor fn) && freePath s fn mycave = [Move fn mycave (costCalc fn mycave)]
      | isCave fn = [Move fn target (costCalc fn target) | target <- path s,
                     (not . isCave) target, freePath s fn target]
      | otherwise = []
      where mycave = caveFor s (visitor fn)

doMove :: Scene -> Move -> Scene
doMove (Scene p lc cc) (Move fn tn cst) = Scene (newPath p) lc (cc + cst)
  where
    newPath [] = []
    newPath (n:ns) | n == fn = fromNode n : newPath ns
                   | n == tn = toNode (visitor fn) n : newPath ns
                   | otherwise = n : newPath ns
    fromNode x | isCave x  = cavePop x
               | otherwise = Step (pos x) Nobody
    toNode a x | isCave x  = cavePush a x
               | otherwise = Step (pos x) a

playOut :: Scene -> [Scene]
playOut sc = concat $ recPlay sc (validMoves sc)
  where
    recPlay s [] | sceneDone s = [[s {lowestCost = min (lowestCost s) (currentCost s)}]]
                 | otherwise   = []
    recPlay s (m:ms) = concat (recPlay ns (validMoves ns)) : recPlay s ms
      where ns = doMove s m

cheapestPlay :: Scene -> Scene
cheapestPlay s = minimumBy (\s1 s2 -> lowestCost s1 `compare` lowestCost s2) $ playOut s
--------------------------

emptyScene :: Scene
emptyScene = Scene [] maxBound 0

initScene :: Path -> Scene
initScene p = emptyScene { path = p }

data Move = Move { from :: Node,
                   to   :: Node,
                   cost :: Cost }
  deriving (Eq)

data Node = Step Pos Amphipod
          | Cave Pos Podtype [Amphipod]
  deriving (Eq)

data Scene = Scene { path :: Path, lowestCost, currentCost :: Cost }
type Path  = [Node]
type Pos   = Int
type Cost  = Int
data Amphipod = Pod { podtype :: Podtype, weight  :: Cost } | Nobody deriving (Eq)
data Podtype  = Amber | Bronze | Copper | Desert deriving (Eq)

sceneDone :: Scene -> Bool
sceneDone = all caveDone . caves

sceneExample = initScene [Step 0 Nobody, Step 1 Nobody, Cave 2 Amber [bronze, amber],
                          Step 3 Nobody, Cave 4 Bronze [copper, desert],
                          Step 5 Nobody, Cave 6 Copper [bronze, copper],
                          Step 7 Nobody, Cave 8 Desert [desert, amber],
                          Step 9 Nobody, Step 10 Nobody]

sceneReal = initScene [Step 0 Nobody, Step 1 Nobody, Cave 2 Amber [bronze, copper],
                       Step 3 Nobody, Cave 4 Bronze [copper, desert],
                       Step 5 Nobody, Cave 6 Copper [amber, desert],
                       Step 7 Nobody, Cave 8 Desert [bronze, amber],
                       Step 9 Nobody, Step 10 Nobody]

sceneReal2 = initScene [Step 0 Nobody, Step 1 Nobody, Cave 2 Amber [bronze, desert, desert, copper],
                        Step 3 Nobody, Cave 4 Bronze [copper, copper, bronze, desert],
                        Step 5 Nobody, Cave 6 Copper [amber, bronze, amber, desert],
                        Step 7 Nobody, Cave 8 Desert [bronze, amber, copper, amber],
                        Step 9 Nobody, Step 10 Nobody]

main = do
  print $ cheapestPlay sceneExample
  print $ cheapestPlay sceneReal
  print $ cheapestPlay sceneReal2

instance Show Scene where
  show s@(Scene p _ _) = nblock (2 + length p) ++ bestCost ++ "\n" ++
                         embrace "#" (concatMap showNodeShort p) ++ nowCost ++ "\n" ++
                         cavelines ++ replicate mlen ' ' ++ nblock cavelinelen
    where
      bestCost  = " Best: " ++ if lowestCost s == maxBound then "-" else show (lowestCost s)
      nowCost   = " Cur:  " ++ show (currentCost s)
      showCaves = map (concatMap show . visitors) $ caves s
      cavelines = (unlines . map (replicate mlen ' ' ++) . transpose . interfork cavewall) showCaves
      cavewall  = (nblock . length . head) showCaves
      cavelinelen = length (caves s) * 2 +1
      mlen      = (length p + 2 - cavelinelen) `div` 2
      nblock    = flip replicate '#'

instance Show Node where
  show (Cave p _ vs) = show p ++ ") Cave: " ++ concatMap show vs
  show (Step p v)    = show p ++ ") Step: " ++ if v == Nobody then "." else show (podtype v)

showNodeShort :: Node -> String
showNodeShort Cave {}                  = "."
showNodeShort (Step _ v) | v == Nobody = "."
                         | otherwise   = show $ podtype v

instance Show Amphipod where
  show (Pod t _) = show t
  show _         = "."

instance Show Podtype where
  show Amber  = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

instance Show Move where
  show (Move fr to cst) = show (pos fr) ++ " -> " ++ show (pos to) ++
                          " (" ++ show cst ++ ")"
instance Ord Move
  where compare m1 m2 = compare (cost m1) (cost m2)

none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p

embrace :: [a] -> [a] -> [a]
embrace x xs = x ++ xs ++ x

interfork :: a -> [a] -> [a]
interfork x xs = embrace [x] (intersperse x xs)

-- replace first matching occurence with new value
replaceFirstP :: (a -> Bool) -> a -> [a] -> [a]
replaceFirstP p y = go
  where
    go [] = []
    go (x:xs) | p x = y : xs
              | otherwise = x : go xs

replaceLastP :: (a -> Bool) -> a -> [a] -> [a]
replaceLastP p y = reverse . replaceFirstP p y . reverse

amber,bronze,copper,desert :: Amphipod
amber  = Pod Amber  1
bronze = Pod Bronze 10
copper = Pod Copper 100
desert = Pod Desert 1000
