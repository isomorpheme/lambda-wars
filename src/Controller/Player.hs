{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Player where

import Config (backfire, bulletSpeed, rotationSpeed, shootDelay, thrustForce)
import qualified Controller.Emitter as Emitter
import Model.Emitter
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
    . updateEmitter movement dt
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
    if action == Shoot && shootCooldown <= 0 then
        (player', Just $ Bullet.create pos direction bulletSpeed)
    else
        (player & _shootCooldown (subtract dt), Nothing)
    where
        player' = player
            & thrust (backfire)
            . set _shootCooldown shootDelay

updateEmitter :: MovementAction -> Float -> Player -> Player
updateEmitter move dt player @ Player { physics = position -> pos, direction } =
    (_emitter $ Emitter.update particle dt) player
    where
        particle = 
            if 
                move == Thrust
            then
                Just $ Particle initialPhysics 
                    { position = pos
                    , velocity = Vector.fromAngleLength direction (-100)
                    } direction 0.3
            else
                Nothing