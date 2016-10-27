{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss

import Model
import Vector (Vector(..))
import qualified Vector

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world @ (World { .. }) =
    drawPlayer player

drawPlayer :: Player -> Picture
drawPlayer Player { position = (Vector x y), direction } =
    Color white
        $ Translate x y
        $ Rotate (Vector.angle' direction) ship
    where
        ship = Scale 20 20 $ Polygon
            [ (0, 1)
            , (0.5, -0.5)
            , (-0.5, -0.5)
            ]
