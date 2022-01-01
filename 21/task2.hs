
{-

Each player turn means 3 rolls with the 3-sided die
For each turn, the current universe splits into 3^3 = 27 new universes
1 universe will have roll-sum 3, 3 universes have roll-sum 4 etc:

(1) 3
(3) 4,4,4
(6) 5,5,5,5,5,5
(7) 6,6,6,6,6,6,6
(6) 7,7,7,7,7,7
(3) 8,8,8
(1) 9

Create a tree where each node
 - has a weight = "in how many universes is this happening"
 - has two players with their respective positions and current scores
 - knows whos turn it is for the next round
 - has a list of nodes representing all sub-universes after the next round

 or

 - is an end-node because either player has won

This creates a tree with all possible paths to reaching a winning score.
Counting universes is done by recursively multiplying the weights of all
nodes in each branch down to the end nodes

It's not very fast and takes a few seconds to run after compiling with ghc -O2

-}

data Player      = Player Name Pos Score deriving Show
data Turn        = PlayerOne | PlayerTwo deriving (Show, Eq)
type Weight      = Int
type SubNodes    = [Node]
type Name        = String
type Pos         = Int
type Score       = Int
type Winner      = Turn

data Node        = Node Weight Player Player Turn SubNodes
                 | GameOver Weight Winner
  deriving Show

winningScore :: Int
winningScore = 21

scoreDistribution :: [(Score, Weight)]
scoreDistribution = zip [3..9] [1,3,6,7,6,3,1]

newNode :: (Player, Player, Turn) -> (Score, Weight) -> Node
newNode (p1, p2, turn) (score, weight)
  | turn == PlayerOne && playerScore (update p1) >= winningScore = GameOver weight PlayerOne
  | turn == PlayerTwo && playerScore (update p2) >= winningScore = GameOver weight PlayerTwo
  | turn == PlayerOne = Node weight (update p1) p2 (next turn)
                        (map (newNode (update p1, p2, PlayerTwo)) scoreDistribution)
  | otherwise         = Node weight p1 (update p2) (next turn)
                        (map (newNode (p1, update p2, PlayerOne)) scoreDistribution)
  where
    update (Player n p s) = Player n (newpos p) (s + newpos p)
    newpos current = (current + score -1) `rem` 10 + 1

playerScore :: Player -> Int
playerScore (Player _ _ s) = s

next :: Turn -> Turn
next PlayerOne = PlayerTwo
next _         = PlayerOne

newTree :: Pos -> Pos -> Node
newTree p1p p2p = Node 1 p1 p2 PlayerOne subNodes
  where
    p1 = Player "Player 1" p1p 0
    p2 = Player "Player 2" p2p 0
    subNodes = map (newNode (p1, p2, PlayerOne)) scoreDistribution

countWinners :: Winner -> Node -> Int
countWinners w (GameOver weight who) | w == who  = weight
                                     | otherwise = 0
countWinners w (Node weight _ _ _ subnodes)
  = weight * sum (map (countWinners w) subnodes)

mainTest :: IO ()
mainTest = print $ countWinners PlayerOne $ newTree 4 8

main :: IO ()
main = do
  let tree = newTree 3 5
  let p1winners = countWinners PlayerOne tree
  let p2winners = countWinners PlayerTwo tree
  print $ max p1winners p2winners
