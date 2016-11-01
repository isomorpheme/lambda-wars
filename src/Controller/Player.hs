{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Player where

import Config (backfire, bulletSpeed, rotationSpeed, shootDelay, thrustForce)
import Model.Player
import Model.World
import Model.Bullet as Bullet
import Physics
import Vector (mul, add)
import qualified Vector
import Util

update :: PlayerActions -> Float -> Player -> (Player, Maybe Bullet)
update (movement, rotation, shoot) dt =
    ( updateShooting shoot dt
    . updateMovement movement dt
    . updateRotation rotation dt
    )

updateMovement :: MovementAction -> Float -> Player -> Player
updateMovement Thrust dt = thrust $ thrustForce * dt
updateMovement NoMovement _ = id

updateRotation :: RotateAction -> Float -> Player -> Player
updateRotation RotateRight dt = rotate $ rotationSpeed * dt
updateRotation RotateLeft dt = rotate $ -rotationSpeed * dt
updateRotation NoRotation _ = id

updateShooting :: ShootAction -> Float -> Player -> (Player, Maybe Bullet)
updateShooting action dt player @ Player { physics = position -> pos, .. } =
    if action == Shoot && cooldown' <= 0 then
        (player', Just $ Bullet.create pos direction bulletSpeed)
    else
        (player & set _shootCooldown cooldown', Nothing)
    where
        cooldown' = shootCooldown - dt
        player' = player
            & thrust (dt * backfire)
            . set _shootCooldown shootDelay
