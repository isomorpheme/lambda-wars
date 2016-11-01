{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Controller.Time
    ( handleTime
    ) where

import Control.Arrow ((>>>))
import Data.List
import System.Random

import Config (rotationSpeed, thrustForce)
import World
import Physics
import Player
import Vector (mul)
import qualified Vector

-- | Time handling

handleTime :: Float -> World -> World
handleTime dt world @ World { .. } =
        ( stepPhysics dt
        . _player (updatePlayer (movementAction, rotateAction, shootAction) dt)
        ) world

updatePlayer :: PlayerAction -> Float -> Player -> Player
updatePlayer (movement, rotation, _) dt player =
    updateMovement . updateRotation $ player
    where
        updateMovement =
            let
                acceleration = case movement of
                    NoMovement ->
                        Vector.zero
                    Thrust ->
                        Vector.fromAngleLength (direction player) thrustForce
            in
                _physics (accelerate $ dt `mul` acceleration)
        updateRotation = case rotation of
            RotateRight ->
                rotate (rotationSpeed * dt)
            RotateLeft ->
                rotate (-rotationSpeed * dt)
            NoRotation ->
                id
