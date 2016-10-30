{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model
import Physics
import Vector (Vector(..))
import qualified Vector

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw _ _ World { .. } =
    drawPlayer player

drawPlayer :: Player -> Picture
drawPlayer Player { physics = (position -> Vector x y), direction } =
    Color white
        $ Translate x y
        $ Rotate (radToDeg direction) ship
    where
        ship = Scale 15 15 $ Polygon
            [ (0, 1)
            , (0.5, -0.5)
            , (-0.5, -0.5)
            ]
