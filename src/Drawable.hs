{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Drawable where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

class Drawable a where
    draw :: a -> Picture

instance Drawable a => Drawable [a] where
    draw xs = Pictures $ map draw xs
