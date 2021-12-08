import Data.List

type Pos = Int

main2 :: IO ()
main2 = do
  input <- map read . splitBy ',' <$> readFile "input.txt" :: IO [Int]
  let mapped = map (\x -> (head x, length x)) . group . sort $ input
  print $ expLowestCost mapped

expLowestCost :: [(Pos, Int)] -> Int
expLowestCost xs = minimum [expPosCost p xs | p <- [0..maximum $ map fst xs]]

expPosCost :: Pos -> [(Pos, Int)] -> Int
expPosCost to [] = 0
expPosCost to ((pos,num):xs) = num * deltaTri to pos + expPosCost to xs

deltaTri :: Pos -> Pos -> Int
deltaTri from to = (d * (d+1)) `div` 2
  where d = abs (from - to)

main1 :: IO ()
main1 = do
  input <- map (read :: String -> Int) . splitBy ',' <$> readFile "input.txt"
  let here = head $ drop (length input `div` 2) $ sort input
  print $ linearCost here input

linearCost :: Int -> [Int] -> Int
linearCost pos = foldr (\a b -> abs (a - pos) + b) 0

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim = foldr f [[]]
  where f c a@(x:xs) | c == delim = []:a
                     | otherwise = (c:x):xs
