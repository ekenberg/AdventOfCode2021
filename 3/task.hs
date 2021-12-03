import Data.List
import Data.Char(digitToInt)

main2 = do
  input <- words <$> readFile "input.txt"
  let (oxygen, co2) = filterRatings input
  print $ oxygen * co2

filterRatings :: [String] -> (Int, Int)
filterRatings xss = (bin2int $ head $ filterRating mostcommonbit  "" xss,
                     bin2int $ head $ filterRating leastcommonbit "" xss)

filterRating :: (String -> Char) -> String -> [String] -> [String]
filterRating _ prefix []     = [prefix]
filterRating _ prefix (x:[]) = [prefix ++ x]
filterRating p prefix xss    = filterRating p (prefix ++ [c]) filtered
  where
    c = p $ head $ transpose xss
    filtered | length xss == 1 = []
             | otherwise       = map tail $ filter (\(x:xs) -> x == c) xss

-------------------------------------------------------------------

-- "110" -> '1', "1100" -> '1' and "010" -> '0'
mostcommonbit :: String -> Char
mostcommonbit xs = if moreones then '1' else '0'
  where
    moreones  = numzeroes * 2 <= length xs
    numzeroes = length $ filter (== '0') xs

leastcommonbit = flipc . mostcommonbit

-- "110" -> 6
bin2int :: String -> Int
bin2int = b2i . reverse
  where
    b2i :: String -> Int
    b2i [] = 0
    b2i (x:xs) = (digitToInt x) + 2 * b2i xs

-------------------------------------------------------------------

main1 = do
  input <- (transpose . words) <$> readFile "input.txt"
  let x = map mostcommonbit input
  print $ (bin2int x * bin2int (bitflip x))


-- "110" -> "001"
bitflip :: String -> String
bitflip = map flipc


flipc :: Char -> Char
flipc c | c == '1' = '0'
        | otherwise = '1'
