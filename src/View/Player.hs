{-# LANGUAGE ViewPatterns #-}

module View.Player where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Model.Player
import Physics

instance Draw Player where
    draw Player { physics = position -> pos, direction } =
        Color white
            $ uncurry Translate pos
            $ Rotate (radToDeg direction) ship
        where
            ship = Scale 3 3 $ lineLoop
                [ (0, 4)
                , (2, -2)
                , (0, -1)
                , (-2, -2)
                ]
