import Debug.Trace (trace)

type Name     = String
type Position = Int
type RollNum  = Int
type Score    = Int

type Roll    = (Score, RollNum)
type Die     = [Roll]

data Player = Player { name  :: String,
                       pos   :: Int,
                       score :: Int }
  deriving Show

data Game = Game { gameDie :: Die,
                   player1 :: Player,
                   player2 :: Player }

instance Show Game where
  show (Game die p1 p2)
    = "Game after " ++ show rn ++ " rolls:\n" ++
      " Player " ++ name p1 ++ ": Pos (" ++
      show (pos p1) ++ ") Score (" ++ show (score p1) ++ ")\n" ++
      " Player " ++ name p2 ++ ": Pos (" ++
      show (pos p2) ++ ") Score (" ++ show (score p2) ++ ")\n" ++
      show minScore ++ " * " ++ show rn ++ " = " ++ show (minScore*rn)
    where
      rn = snd (head die) - 1
      minScore = min (score p1) (score p2)

die :: Die
die = [(i`rem`100 + 1, i+1) | i <- [0..]]

roll3 :: Die -> (Roll, Die)
roll3 die = ((sum3, rc), drop 3 die)
  where
    sum3 = sum $ map fst $ take 3 die
    rc   = snd $ die !! 2

newPlayer :: Name -> Position -> Player
newPlayer n p = Player n p 0

dtrace s f | doTrace = trace s f
           | otherwise = f

play :: Game -> Game
play g@(Game die p1 p2)
  | score p2 >= 1000 = g
  | otherwise = dtrace (show p1 ++ " -> " ++ show p1played ++ "\n  " ++ show (rsum, rn))
                play (Game newDie p2 p1played)
  where
    ((rsum, rn), newDie) = roll3 die
    p1Pos = (pos p1 + rsum -1) `rem` 10 + 1
    p1played = p1 { pos = p1Pos, score = score p1 + p1Pos }

doTrace = False

testRun1 = play $ Game die (newPlayer "1" 4) (newPlayer "2" 8)
run1     = play $ Game die (newPlayer "1" 3) (newPlayer "2" 5)
