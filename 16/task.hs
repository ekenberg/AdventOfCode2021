import Data.Char

{-
Packet format:
 - first 3 bits = packet VERSION (number, ex 100 = 4)
 - next 3 bits  = packet TYPE ID (number, ex 100 = 4)
 - there might be trailing, discardable zeros at the end of a packet

Packet TYPE IDS:
 - ID = 4 => literal value Int
     read (1+4) bits at a time until the first bit = 0
     1xxxx 1yyyy 1zzzz 0pppp -> binary number xxxxyyyyzzzzpppp
 - ID /= 4 => OPERATOR
   if first bit == 0 then
     read 15 bits = number representing how many bits to read containing sub-packets
   else -- first bit == 1
     read 11 bits = number representing how many sub-packets are contained in this packet
   end
-}

type Type    = Int
type Version = Int
type Bin     = String

data Packet = Literal Version Int | Op Version Type [Packet]
  deriving Show

versionSum :: [Packet] -> Int
versionSum [] = 0
versionSum (p:ps) =
  case p of
    Literal v _  -> v + versionSum ps
    Op v _ ps'   -> v + versionSum ps' + versionSum ps

valueSum :: Packet -> Int
valueSum (Literal _ i) = i
valueSum (Op _ t ps) = aggregate (map valueSum ps)
  where
    aggregate = case t of
      0 -> sum
      1 -> product
      2 -> minimum
      3 -> maximum
      5 -> gt
      6 -> lt
      7 -> eq
      _ -> error "uh-oh"
    gt [x,y] = if x > y then 1 else 0
    lt [x,y] = if x < y then 1 else 0
    eq [x,y] = if x == y then 1 else 0

-- parse n packets from indata
nParse :: Int -> Bin -> ([Packet], Bin)
nParse n xs = nRecParse n ([], xs)
  where
    nRecParse 0 (ps, b) = (reverse ps, b) -- fix reversal below
    nRecParse i (ps, b) = nRecParse (i-1) (p : ps, r) -- this reverses order of packets
      where (p, r) = parse b

-- parse until out of indata
recParse :: Bin -> [Packet]
recParse xs | all (== '0') xs = []
            | otherwise       = p : recParse r
  where
    (p, r) = parse xs

parse :: Bin -> (Packet, Bin)
parse xs
  | typeid == 4 = (Literal ver lit, litRest)
  | lt == '0'   = (Op ver typeid (recParse recBin), recRest)
  | otherwise   = (Op ver typeid nParsed, nRest)
  where
    ver     = binToInt $ take 3 xs
    typeid  = binToInt $ take 3 $ drop 3 xs

    -- literal
    lit     = binToInt litBin
    litBin  = readLiteral $ drop 6 xs
    litRest = drop (6 + length litBin + (length litBin `div` 4)) xs
    lt      = xs !! 6

    readLiteral ('1':a:b:c:d:rest) = [a,b,c,d] ++ readLiteral rest
    readLiteral ('0':a:b:c:d:_)    = [a,b,c,d]
    readLiteral _                  = error "what"

    -- parse certain number of bits
    recRead = binToInt $ take 15 $ drop 7 xs
    recBin  = take recRead $ drop (7 + 15) xs
    recRest = drop (7 + 15 + recRead) xs

    -- parse certain number of sub-packet
    nNum    = binToInt $ take 11 $ drop 7 xs
    nBin    = drop (7 + 11) xs
    (nParsed, nRest) = nParse nNum nBin

hexToBin :: String -> String
hexToBin [] = []
hexToBin ('0':xs) = "0000" ++ hexToBin xs
hexToBin ('1':xs) = "0001" ++ hexToBin xs
hexToBin ('2':xs) = "0010" ++ hexToBin xs
hexToBin ('3':xs) = "0011" ++ hexToBin xs
hexToBin ('4':xs) = "0100" ++ hexToBin xs
hexToBin ('5':xs) = "0101" ++ hexToBin xs
hexToBin ('6':xs) = "0110" ++ hexToBin xs
hexToBin ('7':xs) = "0111" ++ hexToBin xs
hexToBin ('8':xs) = "1000" ++ hexToBin xs
hexToBin ('9':xs) = "1001" ++ hexToBin xs
hexToBin ('A':xs) = "1010" ++ hexToBin xs
hexToBin ('B':xs) = "1011" ++ hexToBin xs
hexToBin ('C':xs) = "1100" ++ hexToBin xs
hexToBin ('D':xs) = "1101" ++ hexToBin xs
hexToBin ('E':xs) = "1110" ++ hexToBin xs
hexToBin ('F':xs) = "1111" ++ hexToBin xs
hexToBin _        = error "oops"

binToInt :: String -> Int
binToInt = b2i . reverse
  where
    b2i [] = 0
    b2i (x:xs) = digitToInt x + 2 * b2i xs

main2 = do
  input <- head . lines <$> readFile "input.txt"
  print $ map valueSum $ recParse $ hexToBin input

main = do
  input <- head . lines <$> readFile "input.txt"
  print $ versionSum $ recParse $ hexToBin input
