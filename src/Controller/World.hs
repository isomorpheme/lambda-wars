{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.World where

import Config (spawnTime)
import Controller.Enemy as Enemy
import Controller.Particle as Particle
import Controller.Player as Player
import Model.Enemy as Enemy
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
        , _particles . _physics $ step dt
        ]

updatePlayer :: Float -> World -> World
updatePlayer dt world @ World { player } =
    let (player', bullet, particles) = Player.update (world & playerActions) dt player
    in world
        & set _player player'
        & _bullets (maybe id (:) bullet)
        . _particles (particles ++)

updateEnemies :: Float -> World -> World
updateEnemies dt world @ (player -> physics' -> position -> pos) =
    world & _enemies (map $ Enemy.update pos dt)


updateSpawning :: Float -> World -> World
updateSpawning dt world @ World { .. } =
    if spawnTimer <= 0 then
        world
            & spawnEnemy
            & set _spawnTimer spawnTime
    else
        world & _spawnTimer (subtract dt)

spawnEnemy :: World -> World
spawnEnemy world @ World { .. } =
    let
        playerBounds = player & physics' & bounds
        (enemy, rndGen') = spawn screenBounds playerBounds rndGen
    in
        world { enemies = enemy:enemies, rndGen = rndGen' }

updateParticles :: Float -> World -> World
updateParticles dt world =
    world & _particles updateParticles'
    where
        updateParticles' [] = []
        updateParticles' (x:xs) = maybe id (:) (Particle.update dt x) (updateParticles' xs)