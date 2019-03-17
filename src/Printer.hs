module Printer where

import Data.Tree
import Data.Maybe
import Data.Text.Prettyprint.Doc

import Game hiding (Tree(..))
import Utils

instance Pretty Cell where
    pretty X = pretty 'x' 
    pretty O = pretty 'o'
    pretty B = pretty '.'

instance Pretty Player where
    pretty PlayerX = pretty ("Player X" :: String)
    pretty PlayerO = pretty ("Player O" :: String)

prettyRow r = sep $ zipWith (<+>) (emptyDoc : repeat pipe) (map pretty r)

prettyGrid g = vsep $ interleave (pretty (replicate dashLength '-')) (map prettyRow g)
    where 
        gridSize   = length $ g !! 0
        dashLength = (gridSize - 1) * 4 + 3

convertTree :: GameTree -> Tree (Grid, Player)
convertTree (GtNode p ts) = Node p $ map convertTree (catMaybes ts)

prettyTree t = drawTree converted
    where converted = fmap (\(g,p) -> show $ vsep [pretty p, prettyGrid g]) t

prettyGameTree :: Int -> GameTree -> IO () 
prettyGameTree n = putStrLn . prettyTree . (prune n) . convertTree 

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]
