{-# LANGUAGE RecordWildCards #-}

module Controller.Player where

import Config (backfire, bulletSpeed, rotationSpeed, shootDelay, thrustForce)
import Model.Player
import Model.World
import Model.Bullet
import Physics
import Vector (mul, add)
import qualified Vector

update :: PlayerActions -> Float -> Player -> (Player, Maybe Bullet)
update (movement, rotation, shoot) dt (player @ Player { .. }) =
    (updateMovement . updateRotation . updateCooldown . applyBackfire $ player, maybeBullet)
    where
        updateMovement =
            case movement of
                Thrust ->
                    thrust $ dt * thrustForce
                NoMovement ->
                    id
        updateRotation =
            case rotation of
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
                        ( thrust $ dt * backfire
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
