{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Player where

import Config (backfire, bulletSpeed, rotationSpeed, shootDelay, thrustForce)
import qualified Controller.Particle as Particle
import Model.Particle
import Model.Player
import Model.World
import Model.Bullet as Bullet
import Physics
import Vector (mul, add)
import qualified Vector
import Util

update :: PlayerActions -> Float -> Player -> (Player, Maybe Bullet, [Particle])
update (movement, rotation, shoot) dt player = (player', bullet, particles)
    where
        (player', bullet) =
            ( updateShooting shoot dt
            . updateMovement movement dt
            . updateRotation rotation dt
            ) player
        particles = addParticles movement player        

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

addParticles :: MovementAction -> Player -> [Particle]
addParticles NoMovement _ = []
addParticles _ Player { physics = position -> pos, direction } = 
    Particle initialPhysics
        { position = pos `add` Vector.fromAngleLength direction (-10)
        , velocity = Vector.fromAngleLength direction (-100)
        } direction 0.3
        :[]
