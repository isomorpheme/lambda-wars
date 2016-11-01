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

import Config --(rotationSpeed, thrustForce, bulletSpeed, shootDelay)
import World
import Physics
import Bullet
import Player
import Vector (mul, add)
import qualified Vector

-- | Time handling

handleTime :: Float -> World -> World
handleTime dt world @ World { .. } =
        ( stepPhysics dt
        . _player (const newPlayer)
        . addBullet maybeBullet
        ) world
            where
                (newPlayer, maybeBullet) = 
                    updatePlayer (movementAction, rotateAction, shootAction) dt player

updatePlayer :: PlayerAction -> Float -> Player -> (Player, Maybe Bullet)
updatePlayer (movement, rotation, shoot) dt (player @ Player { .. }) =
    (updateMovement . updateRotation . updateCooldown . applyBackfire $ player, maybeBullet)
    where
        updateMovement =
            let
                acceleration = case movement of
                    NoMovement ->
                        Vector.zero
                    Thrust ->
                        Vector.fromAngleLength direction thrustForce
            in
                _physics (accelerate $ dt `mul` acceleration)
        updateRotation = case rotation of
            RotateRight ->
                rotate (rotationSpeed * dt)
            RotateLeft ->
                rotate (-rotationSpeed * dt)
            NoRotation ->
                id
        (applyBackfire, updateCooldown, maybeBullet) = 
            case shoot of
                Shoot ->
                    if 
                        shootCooldown == 0
                    then 
                        ( _physics (accelerate $ (Vector.fromAngleLength direction backfire))
                        , _shootCooldown $ const shootDelay
                        , Just $ Bullet physics { velocity = velocity physics `add` Vector.fromAngleLength direction bulletSpeed } direction
                        )
                    else 
                        dontShoot
                DontShoot ->
                    dontShoot
        dontShoot =
            ( id
            , _shootCooldown $ \x -> max 0 (x - dt)
            , Nothing
            )