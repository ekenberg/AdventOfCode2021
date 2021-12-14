import Data.List hiding (nub)
import Data.Char
import Debug.Trace

type From = String
type To   = String
type Passage = (From, To) -- passage between two caves
type Path = [Passage]     -- path of matching passages

explore2 :: [Passage] -> [Path] -> [Path]
explore2 _ []        = []
explore2 passages ps = nub $ filter done $ ended ++ new_paths ++ explore2 passages new_paths
  where
    ended           = filter done ps
    new_paths       = [p `add` s | p <- ps, s <- possibleSteps p]
    possibleSteps p = filter (canEnter2 p) passages
    add p s = p ++ [s]

canEnter2 :: Path -> Passage -> Bool
canEnter2 p passage@(from, to)
  | tip /= from              = False          -- not even there
  | to == "start"            = False          -- cannot revisit start
  | leadsToSmallCave passage = canEnterSmallCave
  | otherwise                = True           -- can revisit large caves
  where
    didVisit              = to `elem` (map snd p)
    canEnterSmallCave     = (not $ didVisit) || onlySingleSmallVisits
    onlySingleSmallVisits = all (<2) $ map length $ group $ sort $ map snd $ filter (all isLower . snd) p
    tip                   = snd $ last p

explore :: [Passage] -> [Path] -> [Path]
explore _ []        = []
explore passages ps = nub $ filter done $ ended ++ new_paths ++ explore passages new_paths
  where
    ended           = filter done ps
    new_paths       = [p `add` s | p <- ps, s <- possibleSteps p]
    possibleSteps p = filter (canEnter p) passages
    add p s = p ++ [s]

done :: Path -> Bool
done = (=="end") . snd . last

leadsToSmallCave :: Passage -> Bool
leadsToSmallCave (from, to) = all isLower to

-- can this passage be entered from path?
canEnter :: Path -> Passage -> Bool
canEnter p passage@(from, to)
  | tip /= from              = False          -- not even there
  | leadsToSmallCave passage = not $ didVisit -- cannot revisit small caves
  | otherwise                = True           -- can revisit large caves
  where
    didVisit = to `elem` (map snd p)
    tip      = snd $ last p

buildInputPassages :: [String] -> [(String, String)]
buildInputPassages [] = []
buildInputPassages (x:xs) | l == "start" = (l, r) : buildInputPassages xs
                          | r == "start" = (r, l) : buildInputPassages xs
                          | l == "end"   = (r, l) : buildInputPassages xs
                          | r == "end"   = (l, r) : buildInputPassages xs
                          | otherwise    = (l, r) : (r, l) : buildInputPassages xs
  where (l, r) = splitOn '-' x

startingPaths :: [Passage] -> [Path]
startingPaths = group . filter ((=="start") . fst)

showPath :: Path -> String
showPath p = "start," ++ (concat $ intersperse "," $ map snd p)

main2 :: IO ()
main2 = do
  passages <- nub . buildInputPassages . lines <$> readFile "input.txt"
  --putStrLn $ unlines $ map show passages
  let paths = explore2 passages $ startingPaths passages
  putStrLn $ show (length paths) ++ " possible paths"
  --putStrLn $ unlines $ map showPath paths

main1 :: IO ()
main1 = do
  passages <- nub . buildInputPassages . lines <$> readFile "input.txt"
  --putStrLn $ unlines $ map show passages
  let paths = explore passages $ startingPaths passages
  putStrLn $ show (length paths) ++ " possible paths"
  --putStrLn $ unlines $ map show $ paths

----------------------
splitOn :: Char -> String -> (String, String)
splitOn c xs = (takeWhile (/=c) xs, tail $ dropWhile (/=c) xs)

nub :: Ord a => [a] -> [a]
nub = map head . group . sort
