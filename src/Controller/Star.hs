{-# LANGUAGE RecordWildCards #-}

module Controller.Star where

import Config
import Model.Star
import Rectangle
import Vector
import Util

-- | Updates the position of a star based on its depth and wraps it around the screen.
update :: Float -> Star -> Star
update dt star @ Star { .. } =
    star
        & _position (+velocity)
        & _position (wrap rect)
        where
            velocity = fromAngleLength starAngle $ starSpeed / depth
            rect = rectangle 0 (cameraWidth, cameraHeight)
