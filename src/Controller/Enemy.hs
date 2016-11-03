{-# LANGUAGE RecordWildCards #-}

module Controller.Enemy where

import Config (seekerSpeed)
import Model.Enemy
import Physics
import Vector
import Util

update :: Point -> Float -> Enemy -> Enemy
update playerPosition dt enemy @ Enemy { .. } =
    enemy & _physics (accelerate a)
    where
        a = (dt * seekerSpeed) `mul` toPlayer
        toPlayer = Vector.normalize $ playerPosition - (physics & position)
