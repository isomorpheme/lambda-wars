module Draw where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

-- | Anything that can be converted into a 'Picture'
class Draw a where
    draw :: a -> Picture

-- | Lists of drawables can also be drawn.
instance Draw a => Draw [a] where
    draw xs = Pictures $ map draw xs
