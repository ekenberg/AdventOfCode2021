import Data.Char

data FishTree a = Pair (FishTree a) (FishTree a) | Reg a

data Token = LB | RB | LN Int | RN Int
  deriving (Show, Eq)

data PairPos = L | R

newtype FishNum = FishNum [Token]
  deriving Eq

instance (Show a) => Show (FishTree a) where
  show (Reg n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b  ++ "]"

instance (Read a) => Read (FishTree a) where
  readsPrec _ ('[':rest) = [(Pair l r, "")]
    where
      (l, r) = parseFishTrees rest
  readsPrec _ input = [(Reg (read input), "")]

parseFishTrees :: (Read a) => String -> (FishTree a, FishTree a)
parseFishTrees xs = (read l, read r)
  where
    l = gli 0 xs
    r = gli 0 (drop (length l + 1) xs)
    gli n (c:cs) | n > 0  && c == ']' = c : gli (n-1) cs
                 | n == 0 && c == ']' = []
                 | n == 0 && c == ',' = []
                 | c == '['           = c : gli (n+1) cs
                 | otherwise          = c : gli n cs

instance Read FishNum where
  readsPrec _ str = [(FishNum (readTokens str), "")]
    where
      readTokens [] = []
      readTokens ('[':rest) = LB : readTokens rest
      readTokens (']':rest) = RB : readTokens rest
      readTokens (',':rest) = token : readTokens rest'
        where
          (token, rest') | head rest == '[' = (LB, tail rest)
                         | otherwise        = (RN (read digits :: Int), nonDigits)
          (digits, nonDigits) = span isDigit rest
      readTokens input = LN (read digits :: Int) : readTokens nonDigits
        where
          (digits, nonDigits) = span isDigit input

instance Show FishNum where
  show (FishNum ts) = showTokens ts
    where
      showTokens [] = []
      showTokens (LB:rest) = '[' : showTokens rest
      showTokens (RB:rest)
        | null rest       = "]"
        | head rest == RB = ']' : showTokens rest
        | otherwise       = ']' : ',' : showTokens rest
      showTokens (LN n:rest) = show n ++ ',' : showTokens rest
      showTokens (RN n:rest) = show n ++ showTokens rest

toTree :: FishNum -> FishTree Int
toTree = read . show

magnitude :: FishNum -> Int
magnitude = magTree . toTree
  where
    magTree (Reg n) = n
    magTree (Pair a b) = 3 * magTree a + 2 * magTree b

add :: FishNum -> FishNum -> FishNum
add (FishNum x) (FishNum []) = FishNum x
add (FishNum []) (FishNum y) = FishNum y
add (FishNum x) (FishNum y) = reduce $ FishNum (LB : x ++ y ++ [RB])

explode :: FishNum -> FishNum
explode (FishNum ts) = ex L 0 [] ts
  where
    ex :: PairPos -> Int -> [Token] -> [Token] -> FishNum
    ex _ _ l []          = FishNum (reverse l)

    ex p n l (LB:LN x:RN y:RB:r)
      | n < 4            = ex R n (RB:RN y:LN x:LB:l) r
      | otherwise        = FishNum (lx ++ [nn] ++ rx)
      where
        lx = reverse $ addFirst x l
        rx = addFirst y r
        nn = case p of
               L -> LN 0
               R -> RN 0
    ex _ n l (LB:LN x:r) = ex R (n+1) (LN x:LB:l) r
    ex _ n l (LN x:LB:r) = ex R (n+1) (LB:LN x:l) r
    ex p n l (RN x:RB:r) = ex p (n-1) (RB:RN x:l) r
    ex p n l (RB:r)      = ex p (n-1) (RB:l) r
    ex _ n l (LB:r)      = ex L (n+1) (LB:l) r

addFirst :: Int -> [Token] -> [Token]
addFirst _ [] = []
addFirst x (LN v:ts) = LN (v+x) : ts
addFirst x (RN v:ts) = RN (v+x) : ts
addFirst x (t:ts) = t : addFirst x ts

split :: FishNum -> FishNum
split (FishNum ts) = FishNum (sp ts)
  where
    sp [] = []
    sp (LN x:r) | x >= 10 = pr x r
                | otherwise = LN x : sp r
    sp (RN x:r) | x >= 10 = pr x r
                | otherwise = RN x : sp r
    sp (t:ts)   = t : sp ts
    pr x r = LB : LN (x `div` 2) : RN ((x+1) `div` 2) : RB : r

reduce :: FishNum -> FishNum
reduce fn | exploded /= fn = reduce exploded
          | split'   /= fn = reduce split'
          | otherwise      = fn
  where
    exploded = explode fn
    split'   = split fn

empty :: FishNum
empty = FishNum []

readFishNum :: String -> FishNum
readFishNum = read

main :: IO ()
main = do
  numbers <- map readFishNum . lines <$> readFile "input.txt"
  let result = foldl add empty numbers
  print $ magnitude result
  let result2 = [magnitude (add a b) | a <- numbers, b <- numbers, a /= b]
  print $ maximum result2

-- n0  = readFishNum "[9,1]"
-- n00 = readFishNum "[[9,1],[1,9]]"
-- n1  = readFishNum "[[[[[9,8],1],2],3],4]"
-- n10 = readFishNum "[[[[0,9],2],3],4]"
-- n2  = readFishNum "[7,[6,[5,[4,[3,2]]]]]"
-- n3  = readFishNum "[[6,[5,[4,[3,2]]]],1]"
-- n4  = readFishNum "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- n5  = readFishNum "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

-- n6  = readFishNum "[10,1]"
-- n7  = readFishNum "[[10,1],[1,21]]"
-- --
-- e1 = readFishNum "[[[[4,3],4],4],[7,[[8,4],9]]]"
-- e2 = readFishNum "[1,1]"
