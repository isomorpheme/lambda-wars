{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Player where

import Config (backfire, bulletSpeed, rotationSpeed, shootDelay, thrustForce)
import qualified Controller.Particle as Particle
import Model.Particle
import Model.Player as Player
import Model.World
import Model.Bullet as Bullet
import Physics
import Vector
import Util

update :: PlayerActions -> Float -> Player -> (Player, Maybe Bullet, Maybe Particle)
update (movement, rotation, shoot) dt player = (player'', bullet, particle)
    where
        (player'', bullet) =
            ( updateShooting shoot dt
            . updateMovement movement dt
            . updateRotation rotation dt
            ) player'
        (player', particle) = addParticle movement dt player

updateMovement :: MovementAction -> Float -> Player -> Player
updateMovement Thrust dt = thrust $ thrustForce * dt
updateMovement NoMovement _ = id

updateRotation :: RotateAction -> Float -> Player -> Player
updateRotation RotateRight dt = Player.rotate $ rotationSpeed * dt
updateRotation RotateLeft dt = Player.rotate $ -rotationSpeed * dt
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

addParticle :: MovementAction -> Float -> Player -> (Player, Maybe Particle)
addParticle move dt player @ Player { physics = position -> pos, .. } =
    if move == Thrust && exhaustCooldown <= 0 then
        ( player & set _exhaustCooldown 0.01 -- TODO: Random??
        , Just $ Particle initialPhysics
            { position = pos + fromAngleLength direction (-10)
            , velocity = fromAngleLength direction (-100)
            } direction 0.3
        )
    else
        (player & _exhaustCooldown (subtract dt), Nothing)
