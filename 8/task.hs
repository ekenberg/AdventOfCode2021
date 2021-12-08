import Data.List

type PMap = [(String, Char)]

data Parser = P {
  order :: String,
  keys  :: PMap,
  input :: [String]
  } deriving (Show)

main2 :: IO ()
main2 = do
  indata <- map (map words . splitBy '|') . lines <$> readFile "input.txt"
  print $ sum $ map parseRow indata

parseRow :: [[String]] -> Int
parseRow xss = (read :: String -> Int) $ decodeDigits (last xss) map
  where
    -- this seriously looks like I should learn to make my own monads
    map = keys $
          parseByLen 5 $
          parseByCountOverlap '6' 5 $
          parseByOverlap '7' $
          parseByLen 6 $
          parseByLenAndNotOverlap 6 '4' $
          parseByLenAndNotOverlap 6 '1' $
          parseByLen 7 $
          parseByLen 3 $
          parseByLen 4 $
          parseByLen 2 $
          P { order = "1478609352", keys = [],
              input = head xss }


decodeDigits :: [String] -> PMap -> String
decodeDigits [] _ = []
decodeDigits (x:xs) m = cByKey (sort x) m : decodeDigits xs m

parseByCountOverlap :: Char -> Int -> Parser -> Parser
parseByCountOverlap c n p@(P os ks xs) = parse (\x -> countOverlap x (keyByC c ks) == n) p

parseByOverlap :: Char -> Parser -> Parser
parseByOverlap c p@(P os ks xs) = parse (\x -> overlap x (keyByC c ks)) p

parseByLenAndNotOverlap :: Int -> Char -> Parser -> Parser
parseByLenAndNotOverlap n c p@(P os ks xs) = parse (\x -> (length x == n)
                                                     && not (overlap x (keyByC c ks))) p

cByKey :: String -> PMap -> Char
cByKey _ [] = error "woops"
cByKey s ((s', c):ks) | s == s'   = c
                      | otherwise = cByKey s ks

keyByC :: Char -> PMap -> String
keyByC _ [] = error "woops"
keyByC c ((s, c'):ks) | c == c'   = s
                      | otherwise = keyByC c ks

parseByLen :: Int -> Parser -> Parser
parseByLen n = parse ((n==) . length)

parse :: (String -> Bool) -> Parser -> Parser
parse f (P (o:os) ks xs) = P os ((sort key, o) : ks) rest
  where (key, rest) = (head $ filter f xs, filter (not . f) xs)


countOverlap :: String -> String -> Int
countOverlap a b | length a < length b = co (sort a) (sort b)
                 | otherwise           = co (sort b) (sort a)
  where
    co [] _ = 0
    co (x:xs) ys | x `elem` ys = 1 + co xs ys
                 | otherwise   = 0

overlap :: String -> String -> Bool
overlap a b = countOverlap a b == min (length a) (length b)

main1 :: IO ()
main1 = do
  input <- map (map words . splitBy '|') . lines <$> readFile "input.txt"
  print $ countEasy input

countEasy :: [[[String]]] -> Int
countEasy = foldr (\[_,x] s -> s + length (filter (\p -> length p `elem` [2,3,4,7]) x)) 0

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim = foldr f [[]]
  where f c a@(x:xs) | c == delim = []:a
                     | otherwise  = (c:x):xs
