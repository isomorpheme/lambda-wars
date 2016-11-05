{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Player where

import Control.Monad.State
import System.Random

import Config (backfire, bulletSpeed, rotationSpeed, shootDelay, thrustForce)
import qualified Controller.Particle as Particle
import Model.Enemy
import Model.Particle
import Model.Player as Player
import Model.World
import Model.Bullet as Bullet
import Physics
import Vector
import Util

update :: RandomGen g => PlayerActions -> g -> Float -> Player -> (Player, Maybe Bullet, Maybe Particle, g)
update (movement, rotation, shoot) rndGen dt player = (player'', bullet, particle, rndGen')
    where
        (player'', bullet) =
            ( updateShooting shoot dt
            . updateMovement movement dt
            . updateRotation rotation dt
            ) player'
        (player', particle, rndGen') = addParticle movement rndGen dt player

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
        (player', Just $ Bullet.create (pos + fromAngleLength direction 10) direction bulletSpeed)
    else
        (player & _shootCooldown (subtract dt), Nothing)
    where
        player' = player
            & thrust (backfire)
            . set _shootCooldown shootDelay

addParticle :: RandomGen g => MovementAction -> g -> Float -> Player -> (Player, Maybe Particle, g)
addParticle move rndGen dt player @ Player { physics = position -> pos, .. } =
    if move == Thrust && exhaustCooldown <= 0 then
        ( player & set _exhaustCooldown coolDown
        , Just $ Particle initialPhysics
            { position = pos + fromAngleLength direction (-10)
            , velocity = fromAngleLength direction' (-100)
            } direction' 0.3
        , rndGen'
        )
    else
        (player & _exhaustCooldown (subtract dt), Nothing, rndGen)
        where
            ((direction', coolDown), rndGen') = (rndGen &) $
                runState $ do
                    d <- getRandomR (-0.5, 0.5)
                    c <- getRandomR (0.005, 0.02)
                    return (direction + d, c)