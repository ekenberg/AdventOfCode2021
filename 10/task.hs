import Data.List
import Data.Maybe

data LineType = Corrupted Char | Incomplete Braces | Complete
  deriving (Show, Eq)

type Brace  = Char
type Braces = [Brace]

-- order matters in the following lines
openingBraces    = "[<({"
closingBraces    = "]>)}"
corruptedScores  = [57, 25137, 3, 1197]
incompleteScores = [2, 4, 1, 3]

classifyLine :: String -> LineType
classifyLine = braceMatch []

braceMatch :: Braces -> String -> LineType
braceMatch [] [] = Complete
braceMatch os [] = Incomplete os
braceMatch openers (x:xs)
  | isOpener x   = braceMatch (x:openers) xs
  | null openers = Corrupted x
  | match b x    = braceMatch bs xs
  | otherwise    = Corrupted x
  where
    (b, bs)      = (head openers, tail openers)

isOpener :: Brace -> Bool
isOpener b = b `elem` openingBraces

matchingBrace :: Brace -> Brace
matchingBrace b = fromJust $ lookup b $ zip openingBraces closingBraces

match :: Brace -> Brace -> Bool
match a b | isOpener a = matchingBrace a == b
          | otherwise  = matchingBrace b == a

isCorrupted :: LineType -> Bool
isCorrupted (Corrupted _) = True
isCorrupted _             = False

corruptedScore :: LineType -> Int
corruptedScore (Corrupted c) = fromJust $ lookup c $ zip closingBraces corruptedScores
corruptedScore _             = 0

incompleteRest :: LineType -> Braces
incompleteRest (Incomplete openers) = map matchingBrace openers
incompleteRest _ = ""

incompleteScore :: Int -> Braces -> Int
incompleteScore s [] = s
incompleteScore s (b:bs) = incompleteScore (5 * s + score) bs
  where
    score = fromJust $ lookup b $ zip closingBraces incompleteScores

middle :: [a] -> a
middle xs = head $ drop (length xs `div` 2) xs

main2 :: IO ()
main2 = do
  lns <- lines <$> readFile "input.txt"
  print $ middle $ sort $ map (incompleteScore 0) $ filter (not . null) $ map (incompleteRest . classifyLine) lns

main1 :: IO ()
main1 = do
  lns <- lines <$> readFile "input.txt"
  print $ sum $ map (corruptedScore . classifyLine) lns
