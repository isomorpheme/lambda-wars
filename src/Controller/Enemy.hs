{-# LANGUAGE RecordWildCards #-}

module Controller.Enemy where

import Config
import Model.Enemy
import Physics
import Vector
import Util

update :: Point -> Float -> Enemy -> Enemy
update playerPosition dt enemy @ Enemy { .. } =
    case enemyType of
        Seeker -> enemy & _physics (accelerate a)
        Asteroid size rotation rotationSpeed ->
            enemy { enemyType = Asteroid size (rotation + rotationSpeed * dt) rotationSpeed }
        where
            a = (dt * seekerSpeed) `mul` toPlayer
            toPlayer = Vector.normalize $ playerPosition - (physics & position)
