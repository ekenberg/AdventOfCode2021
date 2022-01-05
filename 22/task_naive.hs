import qualified Data.Map as Map

data Action = On | Off deriving (Show, Eq)
data Instruction = Run Action Xrange Yrange Zrange deriving Show
type Xrange = [Int]
type Yrange = [Int]
type Zrange = [Int]
type Reactor = Map.Map (Int, Int, Int) Bool
type Filter = ([Int] -> [Int])

emptyReactor :: Reactor
emptyReactor = Map.fromList []

runInstruction :: Reactor -> Instruction -> Reactor
runInstruction r (Run a xs ys zs)
  | a == On   = foldr (\k m -> Map.insert k True m) r range
  | otherwise = foldr (\k m -> Map.delete k m) r range
  where
    range = [(x, y, z) | x<-xs, y<-ys, z<-zs]

readInstructions :: Filter -> String -> Instruction
readInstructions flt str = Run action (flt $ pr xrange) (flt $ pr yrange) (flt $ pr zrange)
  where
    action       = if cmd == "on" then On else Off
    (cmd, rst)   = break (==' ') str
    (xrange, r2) = break (==',') $ drop 3 rst
    (yrange, r3) = break (==',') $ drop 3 r2
    (zrange, _)  = break (==',') $ drop 3 r3

fr50 :: Filter
fr50 = filter (\x -> x >= (-50) && x <= 50)

-- parse range
pr :: String -> [Int]
pr s = [read left .. read right]
  where
    (left, r1) = break (=='.') s
    right      = drop 2 r1

dtrace m f = f

main :: IO ()
main = do
  lns <- map (readInstructions fr50) . lines <$> readFile "input.txt"
  let res = foldl (\r a -> runInstruction r a) emptyReactor lns

  print $ Map.size res
