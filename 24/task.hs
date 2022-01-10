import Data.Char
import Data.List
import Debug.Trace (traceShow, trace)
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Leaving everything in here for future reference.
-- | I ended up doing the final calculations with pen and paper,
-- | see main function below

data State = State { w, x, y, z :: Int } deriving (Show, Eq)
data Var   = W | X | Y | Z | Val Int deriving (Eq, Show)
data Op    = Mul Var Var
           | Add Var Var
           | Mod Var Var
           | Div Var Var
           | Eql Var Var
  deriving (Show)

newState :: State
newState = State 0 0 0 0

op :: State -> Op  -> State
op s (Mul a b) = set s a ((varToFunc a) s * (varToFunc b) s)
op s (Add a b) = set s a ((varToFunc a) s + (varToFunc b) s)
op s (Mod a b) | (varToFunc a) s < 0 || (varToFunc b) s <= 0 = error "ILLEGAL MOD"
               | otherwise = set s a ((varToFunc a) s `rem` (varToFunc b) s)
op s (Div a b) = set s a ((varToFunc a) s `div` (varToFunc b) s)
op s (Eql a b) = set s a (if (varToFunc a) s == (varToFunc b) s then 1 else 0)

set :: State -> Var -> Int -> State
set s v val | v == W = s { w = val }
            | v == Y = s { y = val }
            | v == X = s { x = val }
            | v == Z = s { z = val }
            | otherwise = error "ILLEGAL SET"

varToFunc :: Var -> (State -> Int)
varToFunc W = w
varToFunc X = x
varToFunc Y = y
varToFunc Z = z
varToFunc (Val i) = const i

charToVar :: Char -> Var
charToVar 'w' = W
charToVar 'x' = X
charToVar 'y' = Y
charToVar 'z' = Z
charToVar c   = error ("cannot handle char '" ++ [c] ++ "'")

argToVar :: String -> Var
argToVar arg@(c:_) | isDigit c || c == '-' = Val (read arg)
                   | otherwise = charToVar c

readOp :: String -> Op
readOp ('m':'u':'l':' ':o:' ':arg) = Mul (charToVar o) (argToVar arg)
readOp ('a':'d':'d':' ':o:' ':arg) = Add (charToVar o) (argToVar arg)
readOp ('m':'o':'d':' ':o:' ':arg) = Mod (charToVar o) (argToVar arg)
readOp ('d':'i':'v':' ':o:' ':arg) = Div (charToVar o) (argToVar arg)
readOp ('e':'q':'l':' ':o:' ':arg) = Eql (charToVar o) (argToVar arg)
readOp o                           = error ("Cannot Parse: " ++ o)

readSectionN :: Int -> [String] -> [Op]
readSectionN i lns = map readOp (getBlock (i-1) lns)
  where
    getBlock _ [] = []
    getBlock 0 xs = takeWhile notInp (tail xs)
    getBlock n xs = getBlock (n-1) (dropWhile notInp (tail xs))
    notInp = not . ("inp" `isPrefixOf`)

runSection :: Int -> [Op] -> State -> State
runSection n ops s = foldl op (s {w=n}) ops

initState :: Int -> State
initState i = newState {w = i}

main = do
  instructions <- lines <$> readFile "input.txt"
  let s1 = readSectionN 1 instructions
  let s2 = readSectionN 2 instructions
  let s3 = readSectionN 3 instructions
  let s4 = readSectionN 4 instructions
  let s5 = readSectionN 5 instructions
  let s6 = readSectionN 6 instructions
  let s7 = readSectionN 7 instructions
  let s8 = readSectionN 8 instructions
  let s9 = readSectionN 9 instructions
  let s10 = readSectionN 10 instructions
  let s11 = readSectionN 11 instructions
  let s12 = readSectionN 12 instructions
  let s13 = readSectionN 13 instructions
  let s14 = readSectionN 14 instructions
  --mapM_ print s2
  --mapM_ print [runSection (initState i) s1 | i <- [1..9]]
  --mapM_ print [runSection i s2 newState | i <- [1..9]]
  -- mapM_ print [(q == sec2 i j, q)|
  --              i <- [1..9], j <- [1..9],
  --              q <- [runSection j s2 $ runSection i s1 newState]]
  -- mapM_ print [(q == sec3 i j k, q)|
  --              i <- [1..9], j <- [1..9], k <- [1..9],
  --              q <- [runSection k s3 $ runSection j s2 $ runSection i s1 newState]]
  -- let ts4 = [(q == sec4 i j k l, q)|
  --             i <- [1..9], j <- [1..9], k <- [1..9], l <- [1..9],
  --             q <- [runSection l s4 $ runSection k s3 $ runSection j s2
  --                    $ runSection i s1 newState]]
  -- let ts5 = [(q == sec5 i j k l m, q)|
  --             i <- [1..9], j <- [1..9], k <- [1..9], l <- [1..9], m <- [1..9],
  --             q <- [runSection m s5 $ runSection l s4 $ runSection k s3
  --                   $ runSection j s2 $ runSection i s1 newState]]
  -- let ts6 = [(q == sec6 i j k l m n, q)|
  --             i <- [1..9], j <- [1..9], k <- [1..9], l <- [1..9], m <- [1..9], n <- [1..9],
  --             q <- [runSection n s6 $ runSection m s5 $ runSection l s4
  --                   $ runSection k s3 $ runSection j s2 $ runSection i s1 newState]]
  -- let ts7 = [(q == sec7 i j k l m n o, q)|
  --             i <- [1..9], j <- [1..9], k <- [1..9], l <- [1..9], m <- [1..9],
  --             n <- [1..9], o <- [1..9],
  --             q <- [runSection o s7 $ runSection n s6 $ runSection m s5 $ runSection l s4
  --                   $ runSection k s3 $ runSection j s2 $ runSection i s1 newState]]
  -- let ts8 = [(q == sec8 i j k l m n o p, q)|
  --             i <- [1..9], j <- [1..9], k <- [1..9], l <- [1..9], m <- [1..9],
  --             n <- [1..9], o <- [1..9], p <- [1..9],
  --             q <- [runSection p s8 $ runSection o s7 $ runSection n s6 $ runSection m s5
  --                   $ runSection l s4 $ runSection k s3 $ runSection j s2 $ runSection i s1 newState]]
  -- print $ all fst ts8

  -- testMax 10000

  -- print $ monadCheck [1,3,5,7,9,2,4,6,8,9,9,9,9,9]

  {-
   I ended up doing the calculations with pen and paper, coming up with
   the following conditions. Denoting input numbers by r1, r2 ... r14

   r5  = r4-1
   r6  = r3-4
   r8  = r7+8
   r10 = r9+4
   r12 = r11+3
   r13 = r2+1
   r14 = r1-2

   Using these conditions it's easy to figure out max/min:
   -}
  print $ monadCheck [9,8,9,9,8,5,1,9,5,9,6,9,9,7] -- highest
  print $ monadCheck [3,1,5,2,1,1,1,9,1,5,1,4,2,1] -- lowest

  -- print $ head $ filter fst [monadCheck [a,b,c,d,e,f,g,h,i,j,k,l,m,n] |
  --   a <- [9,8..1],
  --   b <- [9,8..1],
  --   c <- [9,8..1],
  --   d <- [9,8..1],
  --   e <- [9,8..1],
  --   f <- [9,8..1],
  --   g <- [9,8..1],
  --   h <- [9,8..1],
  --   i <- [9,8..1],
  --   j <- [9,8..1],
  --   k <- [9,8..1],
  --   l <- [9,8..1],
  --   m <- [9,8..1],
  --   n <- [9,8..1]
  --   ]


monadCheck ps@[r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14] = (z == 0, num)
  where
    (State _ _ _ z) = sec14 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14
    num = concatMap show ps

--- quickCheck testing
newtype Domain = Domain Int deriving (Show)

instance Arbitrary Domain where
  arbitrary = Domain <$> elements [1..9]
  shrink (Domain n) = Domain <$> shrink n

testSec1 :: Int -> IO Bool
testSec1 r1 = do
  instructions <- lines <$> readFile "input.txt"
  let s1 = readSectionN 1 instructions
  return $ sec1 r1 == runSection r1 s1 newState

testSecMax :: Int -> Int -> Int -> Int -> Int ->
              Int -> Int -> Int -> Int -> Int ->
              Int -> Int -> Int -> Int -> IO Bool
testSecMax r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 = do
  instructions <- lines <$> readFile "input.txt"
  let s1 = readSectionN 1 instructions
  let s2 = readSectionN 2 instructions
  let s3 = readSectionN 3 instructions
  let s4 = readSectionN 4 instructions
  let s5 = readSectionN 5 instructions
  let s6 = readSectionN 6 instructions
  let s7 = readSectionN 7 instructions
  let s8 = readSectionN 8 instructions
  let s9 = readSectionN 9 instructions
  let s10 = readSectionN 10 instructions
  let s11 = readSectionN 11 instructions
  let s12 = readSectionN 12 instructions
  let s13 = readSectionN 13 instructions
  let s14 = readSectionN 14 instructions
  return $ sec14 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 ==
    ( runSection r14 s14
    $ runSection r13 s13
    $ runSection r12 s12
    $ runSection r11 s11
    $ runSection r10 s10
    $ runSection r9 s9
    $ runSection r8 s8
    $ runSection r7 s7
    $ runSection r6 s6
    $ runSection r5 s5
    $ runSection r4 s4
    $ runSection r3 s3
    $ runSection r2 s2
    $ runSection r1 s1 newState)

prop_Sec1 :: Domain -> Property
prop_Sec1 (Domain n) = monadicIO $ do
  result <- run $ testSec1 n
  -- monitor (collect n)
  assert result

prop_SecMax :: Domain -> Domain -> Domain -> Domain -> Domain ->
               Domain -> Domain -> Domain -> Domain -> Domain ->
               Domain -> Domain -> Domain -> Domain -> Property
prop_SecMax (Domain r1) (Domain r2) (Domain r3) (Domain r4)
            (Domain r5) (Domain r6) (Domain r7) (Domain r8)
            (Domain r9) (Domain r10) (Domain r11) (Domain r12)
            (Domain r13) (Domain r14)= monadicIO $ do
  result <- run $ testSecMax r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14
  assert result

test1 n = quickCheck (withMaxSuccess n prop_Sec1)
testMax n = quickCheck (withMaxSuccess n prop_SecMax)

--- end testing


sec1 :: Int -> State
--sec1 r1 = State r1 1 (r1+10) (r1+10)
sec1 r1 = State r1 1 0 (r1+10)

sec2 :: Int -> Int -> State
--sec2 r1 r2 = State r2 1 (r2+16) nz
sec2 r1 r2 = State r2 1 0 nz
  where nz = 26 * (r1 + 10) + r2 + 16

--sec3 r1 r2 r3 = State r3 1 r3 nz
sec3 r1 r2 r3 = State r3 1 0 nz
  where nz = 26 * (26 * (r1 + 10) + r2 + 16) + r3

--sec4 r1 r2 r3 r4 = State r4 1 (r4 + 13) nz
sec4 r1 r2 r3 r4 = State r4 1 0 nz
  where nz = 26 * (26 * (26 * (r1 + 10) + r2 + 16) + r3) + r4 + 13

sec5 r1 r2 r3 r4 r5
  | r5 == r4 - 1 = State r5 0 0 (26 * (26 * (r1 + 10) + r2 + 16) + r3)
--  | otherwise    = State r5 1 (r5 + 7) (26 * (26 * (26 * (r1 + 10) + r2 + 16) + r3) + r5 + 7)
  | otherwise    = State r5 1 0 (26 * (26 * (26 * (r1 + 10) + r2 + 16) + r3) + r5 + 7)

sec6 r1 r2 r3 r4 r5 r6
  | r5 == r4-1 && r6 == r3-4 = trace "A" State r6 0 0 (26 * (r1 + 10) + r2 + 16)
--  | r5 == r4-1 = State r6 1 (r6 + 11) (26 * (26 * (r1 + 10) + r2 + 16) + r6 + 11)
  | r5 == r4-1 = trace "B" State r6 1 0 (26 * (26 * (r1 + 10) + r2 + 16) + r6 + 11)
  | r6 == r5+3 = trace "C" State r6 0 0 (26 * (26 * (r1 + 10) + r2 + 16) + r3)
--  | otherwise  = State r6 1 (r6 + 11) (26 * (26 * (26 * (r1 + 10) + r2 + 16) + r3) + r6 + 11)
  | otherwise  = trace "D" State r6 1 0 (26 * (26 * (26 * (r1 + 10) + r2 + 16) + r3) + r6 + 11)

sec7 r1 r2 r3 r4 r5 r6 r7
--  | r7 == pz `mod` 26 + 11 = State r7 0 0 pz
--  = State r7 1 (r7 + 11) (26 * pz + r7 + 11)
  = traceShow pz State r7 1 0 (26 * pz + r7 + 11)
  where
    (State _ _ _ pz) = sec6 r1 r2 r3 r4 r5 r6

sec8 r1 r2 r3 r4 r5 r6 r7 r8
  | r8 == pz `mod` 26 - 3 = State r8 0 0 (pz `div` 26)
--  | otherwise = State r8 1 (r8 + 10) (26 * (pz `div` 26) + r8 + 10)
  | otherwise = State r8 1 0 (26 * (pz `div` 26) + r8 + 10)
  where
    (State _ _ _ pz) = sec7 r1 r2 r3 r4 r5 r6 r7

sec9 r1 r2 r3 r4 r5 r6 r7 r8 r9
--  | r9 == pz `mod` 26 + 12 = State r9 0 0 pz
--  = State r9 1 (r9 + 16) (26 * pz + r9 + 16)
  = State r9 1 0 (26 * pz + r9 + 16)
  where
    (State _ _ _ pz) = sec8 r1 r2 r3 r4 r5 r6 r7 r8

sec10 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10
  | traceShow pz False = undefined
  | r10 == pz `mod` 26 - 12 = State r10 0 0 (pz `div` 26)
--  | otherwise = State r10 1 (r10 + 8) (26 * (pz `div` 26) + r10 + 8)
  | otherwise = State r10 1 0 (26 * (pz `div` 26) + r10 + 8)
  where
    (State _ _ _ pz) = sec9 r1 r2 r3 r4 r5 r6 r7 r8 r9

sec11 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11
--  | r11 == pz `mod` 26 + 13 = State r11 0 0 pz
--  = State r11 1 (r11 + 15) (26 * pz + r11 + 15)
  = State r11 1 0 (26 * pz + r11 + 15)
  where
    (State _ _ _ pz) = sec10 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10

sec12 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12
  | traceShow pz False = undefined
  | r12 == pz `mod` 26 - 12 = State r12 0 0 (pz `div` 26)
--  | otherwise = State r12 1 (r12 + 2) (26 * (pz `div` 26) + r12 + 2)
  | otherwise = State r12 1 0 (26 * (pz `div` 26) + r12 + 2)
  where
    (State _ _ _ pz) = sec11 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11

sec13 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13
  | traceShow pz False = undefined
  | r13 == pz `mod` 26 - 15 = State r13 0 0 (pz `div` 26)
--  | otherwise = State r13 1 (r13 + 5) (26 * (pz `div` 26) + r13 + 5)
  | otherwise = State r13 1 0 (26 * (pz `div` 26) + r13 + 5)
  where
    (State _ _ _ pz) = sec12 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12

sec14 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14
  | traceShow pz False = undefined
  | r14 == pz `mod` 26 - 12 = State r14 0 0 (pz `div` 26)
  | otherwise = State r14 1 (r14 + 10) (26 * (pz `div` 26) + r14 + 10)
  where
    (State _ _ _ pz) = sec13 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13

{-

13:
14: 13 <= pz <= 21 && r14 == pz - 12
-}
