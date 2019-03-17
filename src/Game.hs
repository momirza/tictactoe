module Game (
    Cell(..),
    Grid,
    Player(..),
    Tree(..),
    GameTree(..),
    next,
    showPlayer,
    full,
    gameover,
    wins,
    putGrid,
    bestmoves,
    root,
    move,
    empty,
    gametree
    ) where

import Data.List
import Data.Maybe
import Utils

-- Basic declarations

size :: Int
size = 4        

depth :: Int
depth = 4

winninglength :: Int
winninglength = 4

data Cell = O | B | X
              deriving (Eq, Ord, Show)

type Grid = [[Cell]]

data Player = PlayerO | PlayerX
              deriving (Eq, Show)

data Tree a = Node a [Tree a]
              deriving Show              

data GameTree = GtNode (Grid, Player) [Maybe GameTree]
                deriving Show


-- Grid Utils

-- Example winning grid
-- [[B,O,O],[O,X,O],[X,X,X]] :: Grid

owns :: Player -> Cell -> Bool
owns p c = c == playerCell p


next :: Player -> Player
next PlayerO = PlayerX
next PlayerX = PlayerO

playerCell :: Player -> Cell
playerCell PlayerX = X
playerCell PlayerO = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

won :: Grid -> Bool
won g = wins PlayerO g || wins PlayerX g

gameover :: Grid -> Bool
gameover g = or [won g, full g]

-- we assume player O goes first
turn :: Grid -> Player
turn g = if os <= xs then PlayerO else PlayerX
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g


groups :: Int -> [Cell] -> [[Cell]]
groups _ [] = [[]]
groups n xs = [take n xs'] ++  groups n (drop n xs)
              where xs' = if ((length xs) >= n) then xs else []


consecutive :: Int -> Player -> [Cell] -> Bool
consecutive n p xs = or (map (all (owns p)) (filter (\x -> not (null x)) (groups n xs)))

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = consecutive winninglength p
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Cell]
diag g = [g !! n !! n | n <- [0..size-1]]



-- Display Grid

-- TODO: replace with pretty printer
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Cell] -> [String]
showRow = beside . interleave bar . map showCell
  where
    beside = foldr1 (zipWith (++))
    bar    = replicate 3 "|"

showCell :: Cell -> [String]
showCell O = ["   ", " O ", "   "]
showCell B = ["   ", "   ", "   "]
showCell X = ["   ", " X ", "   "]

showPlayer PlayerX = "Player X"
showPlayer PlayerO = "Player O"


-- Making a move

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: (Grid, Player) -> Int -> Maybe (Grid, Player)
move (g, p) i =
    if valid g i then Just (chop size (xs ++ [c] ++ ys), next p) else Nothing
    where (xs,B:ys) = splitAt i (concat g)
          c = playerCell p


-- Game Trees

root :: GameTree -> (Grid, Player)
root (GtNode a _) = a

gametree :: (Grid, Player) -> GameTree
gametree s = GtNode s [fmap gametree s' | s' <- moves s]

moves :: (Grid, Player) -> [Maybe (Grid, Player)]
moves s@(g, _)
          | gameover g = []
          | otherwise = [move s i | i <- [0..((size^2)-1)]]


-- minimax algorithm
minimax :: Int -> GameTree -> Tree (GameTree, Cell)
minimax d t@(GtNode (g, _) ts)
  | null ts || d == 0 =
                 if | wins PlayerO g  -> Node (t,O) []
                    | wins PlayerX g  -> Node (t,X) []
                    | otherwise -> Node (t,B) []
minimax d t@(GtNode (g, _) ts)
                    | turn g == PlayerO = Node (t, minimum ps) ts'
                    | turn g == PlayerX = Node (t, maximum ps) ts'
                      where
                        ts' = map (minimax (d-1)) (catMaybes ts)
                        ps = [p | Node (_, p) _ <- ts']

bestmoves :: GameTree -> [GameTree]
bestmoves t = [t' | (t',d) <- bestSubtrees, d == minTreeDepth]
               where
                 (_, p)           = root t
                 Node (_,best) ts = minimax depth t
                 bestSubtrees     = [(t', minimum (if (null ts') then [0] else map treedepth ts')) | (Node (t', p') ts') <- ts, p' == best]
                 minTreeDepth     = minimum [d | (_,d) <- bestSubtrees]          


treesize :: Tree a -> Int
treesize (Node _ nodes) = 1 + sum (map treesize nodes) 

treedepth :: Tree a -> Int
treedepth (Node _ []) = 0
treedepth (Node _ nodes) = 1 + maximum (map treedepth nodes)