import Data.List
import Data.List.Split (splitOn, chunksOf)

data Fold    = FoldLeft Int | FoldUp Int deriving Show
type Pos     = (Int, Int) -- (column, row)
type Paper   = [String]   -- ["#..#.", "....#"] etc

foldPaper :: Fold -> Paper -> Paper
foldPaper f p = case f of
  (FoldLeft n) -> chunksOf (pw-n-1) $ join (map (take n) p) (map (reverse . drop (n+1)) p)
  (FoldUp   n) -> chunksOf pw $ join (take n p) (reverse (drop (n+1) p))
  where
    join s1 s2    = [merge a b | (a,b) <- zipLine s1 s2]
    zipLine s1 s2 = zip (concat s1) (concat s2)
    pw            = length $ head p
    merge a b     = if a == '#' || b == '#' then '#' else '.'

initPaper :: [Pos] -> Paper
initPaper coords = [[symbol x y | x <- [0..w]] | y <- [0..h]]
  where
    w = maximum $ map fst coords
    h = maximum $ map snd coords
    symbol x y = if (x,y) `elem` coords then '#' else '.'

printPaper :: Bool -> Paper -> IO ()
printPaper showGrid p = putStrLn ("Num dots: " ++ show numDots ++ "\n" ++ grid)
  where
    numDots = length $ filter (=='#') $ concat p
    grid = if showGrid then unlines p else ""

parseFolds :: [String] -> [Fold]
parseFolds [] = []
parseFolds (c:cs) | "fold along " `isPrefixOf` c = parse (words c !! 2) : parseFolds cs
                     | otherwise = parseFolds cs
  where
    parse ('x':'=':val) = (FoldLeft (read val :: Int))
    parse ('y':'=':val) = (FoldUp   (read val :: Int))

parseCoords :: [String] -> [Pos]
parseCoords [] = []
parseCoords (c:cs) = (col, row) : parseCoords cs
  where
    col = read $ head $ splitOn "," c :: Int
    row = read $ last $ splitOn "," c :: Int

main2 :: IO ()
main2 = do
  lns <- lines <$> readFile "input.txt"
  let coords = parseCoords $ takeWhile (not . null) lns
  let folds  = parseFolds $ tail $ dropWhile (not . null) lns
  --printPaper False $ initPaper coords
  printPaper True $ foldl (\p f -> foldPaper f p) (initPaper coords) folds


main1 :: IO ()
main1 = do
  lns <- lines <$> readFile "input.txt"
  let coords = parseCoords $ takeWhile (not . null) lns
  let folds  = parseFolds $ tail $ dropWhile (not . null) lns
  -- printPaper True $ initPaper coords
  printPaper False $ foldPaper (head folds) $ initPaper coords
