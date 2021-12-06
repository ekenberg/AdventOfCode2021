import Data.List

type Freq = Int

main2 :: IO ()
main2 = do
  input <- readInput "input.txt"
  print $ sum $ apply 256 freqTick $ freqParse input

main1 :: IO ()
main1 = do
  input <- readInput "input.txt"
  print $ sum $ apply 80 freqTick $ freqParse input

readInput :: String -> IO [Int]
readInput f = map (read :: String -> Int) . splitBy ',' . head . lines <$> readFile f

-- [3,4,3,1,2] -> [0,1,1,2,1,0,0,0,0]
freqParse :: [Int] -> [Freq]
freqParse = freqParse' 0 . group . sort
  where
    freqParse' n []  = replicate (9 - n) 0
    freqParse' n xx@(xs:xss)
      | head xs == n = length xs : freqParse' (n+1) xss
      | otherwise    = 0 : freqParse' (n+1) xx

freqTick :: [Int] -> [Int]
freqTick [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,a+h,i,a]

splitBy delim = foldr f [[]]
  where f c a@(x:xs) | c == delim = []:a
                     | otherwise = (c:x):xs

apply :: Int -> (a -> a) -> a -> a
apply 0 _ = id
apply 1 f = f
apply n f = apply (n-1) f . f
