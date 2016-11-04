{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.World where

import Data.Maybe

import Config (spawnTime)
import Controller.Enemy as Enemy
import Controller.Particle as Particle
import Controller.Player as Player
import Model.Enemy as Enemy
import Model.Pickup as Pickup
import Model.Player
import Model.World
import Physics
import Util

stepPhysics :: Float -> World -> World
stepPhysics dt =
    compose
        [ _player . _physics $ step dt
        , _bullets . _physics $ step dt
        , _enemies . _physics $ step dt
        , _pickups . _physics $ step dt
        , _particles . _physics $ step dt
        ]

updatePlayer :: Float -> World -> World
updatePlayer dt world @ World { player } =
    let (player', bullet, particle) = Player.update (world & playerActions) dt player
    in world
        & set _player player'
        & _bullets (maybe id (:) bullet)
        . _particles (maybe id (:) particle)

updateEnemies :: Float -> World -> World
updateEnemies dt world @ (player -> physics' -> position -> pos) =
    world & _enemies (map $ Enemy.update pos dt)

updateEnemySpawning :: Float -> World -> World
updateEnemySpawning dt world @ World { .. } =
    if enemyTimer <= 0 then
        world
            & spawnEnemy
            & set _enemyTimer spawnTime
    else
        world & _enemyTimer (subtract dt)

spawnEnemy :: World -> World
spawnEnemy world @ World { .. } =
    let
        playerBounds = player & physics' & bounds
        (enemy, rndGen') = Enemy.spawn screenBounds playerBounds rndGen
    in
        world { enemies = enemy:enemies, rndGen = rndGen' }

-- TODO: make this bit more DRY

updatePickupSpawning :: Float -> World -> World
updatePickupSpawning dt world @ World { .. } =
    if pickupTimer <= 0 then
        world
            & spawnPickup
            & set _pickupTimer spawnTime
    else
        world & _pickupTimer (subtract dt)

spawnPickup :: World -> World
spawnPickup world @ World { .. } =
    let
        playerBounds = player & physics' & bounds
        (pickup, rndGen') = Pickup.spawn screenBounds playerBounds rndGen
    in
        world { pickups = pickup:pickups, rndGen = rndGen' }

updateParticles :: Float -> World -> World
updateParticles dt world =
    world & _particles (mapMaybe $ Particle.update dt)

updatePlayerCollisions :: World -> World
updatePlayerCollisions world @ World { enemies, player, rndGen } =
    if any (collides player) enemies then
        initial rndGen
    else
        world

updateEnemyCollisions :: World -> World
updateEnemyCollisions world @ World { enemies, bullets, score, multiplier } =
    case go enemies bullets [] [] of
        Just (enemies', bullets') ->
            world
                { enemies = enemies'
                , bullets = bullets'
                , score = score + 1 * multiplier
                }
        Nothing ->
            world
    where
        go [] bs eacc bacc = Nothing
        go (e:es) [] eacc bacc = go es bacc (eacc ++ [e]) []
        go (e:es) (b:bs) eacc bacc
            | collides e b = Just (eacc ++ es, bacc ++ bs)
            | otherwise = go (e:es) bs eacc (bacc ++ [b])
