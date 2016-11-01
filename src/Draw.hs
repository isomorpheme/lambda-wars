{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Draw where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

class Draw a where
    draw :: a -> Picture

instance Draw a => Draw [a] where
    draw xs = Pictures $ map draw xs
