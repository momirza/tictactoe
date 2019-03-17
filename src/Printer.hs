import Data.Text.Prettyprint.Doc

import Game
import Utils

instance Pretty Cell where
    pretty X = pretty 'x' 
    pretty O = pretty 'o'
    pretty B = pretty '.'

prettyRow r = sep $ zipWith (<+>) (emptyDoc : repeat pipe) (map pretty r)

prettyGrid g = vsep $ interleave (pretty (replicate dashLength '-')) (map prettyRow g)
    where 
        gridSize   = length $ g !! 0
        dashLength = (gridSize - 1) * 4 + 3
