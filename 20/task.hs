import Data.List
import qualified Data.IntMap as IMap

data Image = Image { width :: Int,
                     inf   :: Char, -- current state of image infinity
                     bytes :: String }

type Algo = IMap.IntMap Char

----------------- Algo operations
algoFind :: Algo -> String -> Char
algoFind mp s = mp IMap.! bin2int s

nextInf :: Algo -> Image -> Char
nextInf mp im = mp IMap.! bin2int (replicate 9 (inf im))

----------------- Image operations
-- new image by applying f to every 9-bit square
process :: Algo -> (Algo -> String -> Char) -> Image -> Image
process algo f im = Image nw ni (map (f algo) [a++b++c | (a,b,c) <- zs])
  where
    zs  = concat [zip3 x y z | [x,y,z] <- nGroup 3 $ map (nGroup 3) (imgLines src)]
    src = pad 2 (inf im) im
    nw  = width src - 2
    ni  = nextInf algo im

-- add padding around image
pad :: Int -> Char -> Image -> Image
pad n c im = Image nw c $ vframe ++ hframe ++
             intercalate h2frame (imgLines im) ++
             hframe ++ vframe
  where
    nw      = width im + 2*n
    vframe  = concat $ replicate n $ replicate nw c
    hframe  = replicate n c
    h2frame = hframe ++ hframe

imgLines :: Image -> [String]
imgLines (Image w _ bs) = bfold bs
  where
    bfold [] = []
    bfold xs = take w xs : bfold (drop w xs)

imgCountLit :: Image -> Int
imgCountLit = length . filter (== '#') . bytes

----------------- Other
-- ex. nGroup 3 "12345" -> ["123", "234", "345"]
nGroup :: Int -> [a] -> [[a]]
nGroup n xs | length xs >= n = take n xs : nGroup n (tail xs)
            | otherwise      = []

-- "##." -> 6
bin2int :: String -> Int
bin2int = b2i . reverse
  where
    char2int c = if c == '.' then 0 else 1
    b2i [] = 0
    b2i (x:xs) = char2int x + 2 * b2i xs

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

----------------- IO
readImage :: [String] -> Image
readImage s = Image (length lns) '.' (concat lns)
  where lns = drop 2 s

printImage :: Image -> IO ()
printImage = putStr . unlines . imgLines

readAlgo :: [String] -> Algo
readAlgo s = IMap.fromList $ zip [0..] bin
  where bin = head s

main = do
  input <- lines <$> readFile "input.txt"

  let algo  = readAlgo input
  let img   = readImage input
  let pr    = process algo algoFind
  let res2  = applyN 2 pr img
  let res50 = applyN 50 pr img

  print $ imgCountLit res2
  print $ imgCountLit res50
