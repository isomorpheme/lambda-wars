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

import Config (rotationSpeed, movementSpeed, thrustSpeed)
import Model

-- | Time handling

handleTime :: Float -> World -> World
handleTime deltaTime =
    foldr (.) id $ map ($ deltaTime)
        [ handleRotation
        , handleMovement
        ]

handleRotation :: Float -> World -> World
handleRotation deltaTime world @ World { rotateAction, player } =
    world { player = player' }
    where
        player' = case rotateAction of
            RotateRight ->
                rotate (rotationSpeed * deltaTime) player
            RotateLeft ->
                rotate (-rotationSpeed * deltaTime) player
            NoRotation ->
                player

handleMovement :: Float -> World -> World
handleMovement deltaTime world @ World { movementAction, player } =
    world { player = player' }
    where
        player' = case movementAction of
            NoMovement ->
                move (movementSpeed * deltaTime) player
            Thrust ->
                move (thrustSpeed * deltaTime) player
