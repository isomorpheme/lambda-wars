{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Player where

import Control.Monad.State
import System.Random

import Config
import Model.Particle
import Model.Player as Player
import Model.World
import Model.Bullet as Bullet
import Physics
import Vector
import Util

-- | Updates the player and returns particles and bullets if necessary.
update :: RandomGen g => PlayerActions -> g -> Float -> Player -> (Player, Maybe Bullet, Maybe Particle, g)
update (movement, rotation, shoot) rndGen dt player = (player'', bullet, particle, rndGen')
    where
        (player'', bullet) =
            ( updateShooting shoot dt
            . updateMovement movement dt
            . updateRotation rotation dt
            ) player'
        (player', particle, rndGen') = updateParticles movement rndGen dt player

-- | Updates the velocity of the player according to the playerActions.
updateMovement :: MovementAction -> Float -> Player -> Player
updateMovement Thrust dt = thrust $ thrustForce * dt
updateMovement NoMovement _ = id

-- | Updates the rotation of the player according to the playerActions.
updateRotation :: RotateAction -> Float -> Player -> Player
updateRotation RotateRight dt = Player.rotate $ rotationSpeed * dt
updateRotation RotateLeft dt = Player.rotate $ -rotationSpeed * dt
updateRotation NoRotation _ = id

-- | Updates the shootCooldown and returns a bullet and applies knockback to the player if the player shoots.
updateShooting :: ShootAction -> Float -> Player -> (Player, Maybe Bullet)
updateShooting action dt player @ Player { physics = position -> pos, .. } =
    if action == Shoot && shootCooldown <= 0 then
        (player', Just $ Bullet.create (pos + fromAngleLength direction 10) direction bulletSpeed bulletLifeTime)
    else
        (player & _shootCooldown (subtract dt), Nothing)
    where
        player' = player
            & thrust knockback
            . set _shootCooldown shootDelay

-- | Updates the exhaustCooldown and returns new exhaust-particles when the player is thrusting.
updateParticles :: RandomGen g => MovementAction -> g -> Float -> Player -> (Player, Maybe Particle, g)
updateParticles move rndGen dt player @ Player { physics = position -> pos, .. } =
    if move == Thrust && exhaustCooldown <= 0 then
        ( player & set _exhaustCooldown coolDown
        , Just $ Particle defaultPhysics
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
