import Data.List

type Seafloor = [String]

shiftCucumbers :: Seafloor -> Seafloor
shiftCucumbers = transpose . map (shift 'v') . transpose . map (shift '>')

shift :: Char -> String -> String
shift _ [] = []
shift c str@(x:xs) | x == '.' && last xs == c = c : go c (init xs) ++ ['.']
                   | otherwise = go c str
  where
    go :: Char -> String -> String
    go _ [] = []
    go _ [s] = [s]
    go p (s:t:ss) | t == '.' && s == p = '.' : s : go p ss
                  | otherwise = s : go p (t:ss)

exhaust :: Int -> Seafloor -> (Seafloor, Int)
exhaust n sf | sf == ssf = (sf, n)
             | otherwise = exhaust (n+1) ssf
  where
    ssf = shiftCucumbers sf

main :: IO ()
main = do
  sf <- lines <$> readFile "input.txt"
  putStrLn $ unlines sf
  let (nf, n) = exhaust 1 sf
  putStrLn $ unlines nf
  putStrLn $ "Steps: " ++ show n
