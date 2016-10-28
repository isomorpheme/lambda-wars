{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Lens.Micro

import Model
import Vector (Vector(..))
import qualified Vector

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw _ _ world =
    drawPlayer $ world ^. player

drawPlayer :: Player -> Picture
drawPlayer Player { _position = (Vector x y), _direction } =
    Color white
        $ Translate x y
        $ Rotate (Vector.angle' _direction) ship
    where
        ship = Scale 20 20 $ Polygon
            [ (0, 1)
            , (0.5, -0.5)
            , (-0.5, -0.5)
            ]
