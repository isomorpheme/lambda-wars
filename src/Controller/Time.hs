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
handleTime deltaTime =
    foldr (.) id $ map ($ deltaTime)
        [ stepPhysics
        , handleRotation
        , handleMovement
        ]

handleRotation :: Float -> World -> World
handleRotation deltaTime world =
    _player update world
    where
        update = case rotateAction world of
            RotateRight ->
                rotate (rotationSpeed * deltaTime)
            RotateLeft ->
                rotate (-rotationSpeed * deltaTime)
            NoRotation ->
                id

handleMovement :: Float -> World -> World
handleMovement deltaTime world =
    _player update world
    where
        update player =
            _physics (accelerate $ deltaTime `mul` acceleration) player
            where
                acceleration = case movementAction world of
                    NoMovement ->
                        Vector.zero
                    Thrust ->
                        Vector.fromAngleLength (direction player) thrustForce
