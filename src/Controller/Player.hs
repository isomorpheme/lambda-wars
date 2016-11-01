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
