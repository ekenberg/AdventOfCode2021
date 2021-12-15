{-# LANGUAGE TupleSections #-}

import qualified Data.Map as Map

type Pair         = String
type Rule         = (String, Char)
type PairCount    = Map.Map Pair Int -- how many occurences of each type of pair
type TransformMap = Map.Map Pair (Pair, Pair) -- eg "NN" -> ("NC", "CN")

main :: IO ()
main = do
  main1
  main2

makeTransformMap :: [Rule] -> TransformMap
makeTransformMap rs = Map.fromList t
  where
    t = map (\r -> (fst r, ([head (fst r), snd r], [snd r, last (fst r)]))) rs

mergeCount :: [PairCount] -> PairCount
mergeCount = Map.unionsWith (+)

transform :: TransformMap -> PairCount -> PairCount
transform tm pc = mergeCount $ map mp $ Map.assocs pc
  where
    mp (k, v) = Map.fromList [(fst (tlook k), v), (snd $ tlook k, v)]
    tlook k   = tm Map.! k

charCount :: Char -> PairCount  -> [(Char, Int)]
charCount rightMostChar pc = Map.assocs $ Map.fromListWith (+) mp
  where
    mp = (rightMostChar, 1) : map (\(k,v) -> (head k, v)) (Map.assocs pc)

charCountDiff :: [(Char, Int)] -> Int
charCountDiff cis = mx - mn
  where
    mx = maximum $ map snd cis
    mn = minimum $ map snd cis

parsePairs :: String -> [String]
parsePairs []  = []
parsePairs [_] = []
parsePairs (a:b:rest) = [a,b] : parsePairs (b:rest)

parseRules :: [String] -> [Rule]
parseRules = map (\x -> (head $ words x, head $ words x !! 2))

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)


main2 :: IO ()
main2 = run 40

main1 :: IO ()
main1 = run 10

run :: Int -> IO ()
run steps = do
  lns <- lines <$> readFile "input.txt"
  let template    = head lns
  let templateMap = Map.fromListWith (+) $ map (, 1) $ parsePairs template
  let tmap        = makeTransformMap $ parseRules $ drop 2 lns

  let tt          = applyN steps (transform tmap) templateMap
  let cc          = charCount (last template) tt
  print $ charCountDiff cc
