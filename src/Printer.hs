import Data.Text.Prettyprint.Doc

import Game

instance Pretty Cell where
    pretty = viaShow


prettyGrid g = vsep $ map pretty g
