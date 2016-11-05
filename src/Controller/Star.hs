{-# LANGUAGE RecordWildCards #-}

module Controller.Star where

import Config (cameraWidth, cameraHeight, starSpeed, starAngle)
import Model.Star
import Rectangle
import Vector
import Util

update :: Float -> Star -> Star
update dt star @ Star { .. } =
    star 
        & _position (+velocity)
        & _position (wrap rect)
        where
            velocity = fromAngleLength starAngle $ starSpeed / depth
            rect = rectangle 0 cameraWidth cameraHeight