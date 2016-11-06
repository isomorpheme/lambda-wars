{-# LANGUAGE RecordWildCards #-}

module Controller.Enemy where

import Config
import Model.Enemy
import Physics
import Vector
import Util

-- | Updates the velocity of a Seeker to accelerate towards the player.
--   Updates the rotation of an Asteroid.
update :: Point -> Float -> Enemy -> Enemy
update playerPosition dt enemy @ Enemy { .. } =
    case enemyType of
        Seeker -> enemy & _physics (accelerate a)
        Asteroid size rotation rotSpeed ->
            enemy { enemyType = Asteroid size (rotation + rotSpeed * dt) rotSpeed }
        where
            a = (dt * seekerSpeed) `mul` toPlayer
            toPlayer = Vector.normalize $ playerPosition - (physics & position)
