{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.World where

import Control.Monad.State
import Data.List
import Data.Ord
import Data.Maybe

import Config
import Controller.Bullet as Bullet
import Controller.Enemy as Enemy
import Controller.Particle as Particle
import Controller.Player as Player
import Controller.Star as Star
import Model.Enemy as Enemy
import Model.Particle as Particle
import Model.World
import Physics
import Rectangle
import Spawn
import Util
import Vector

-- | Updates the physics of all physical objects in the world.
stepPhysics :: Float -> World -> World
stepPhysics dt =
    compose
        [ _player step'
        , _bullets $ map step'
        , _enemies $ map step'
        , _pickups $ map step'
        , _particles $ map step'
        ]
    where
        step' :: HasPhysics a => a -> a
        step' = _physics $ step dt

-- | Updates the player according to the playerActions and adds possible new bullets and particles to the world.
updatePlayer :: Float -> World -> World
updatePlayer dt world @ World { player , rndGen } =
    let
        (player', bullet, particle, rndGen') =
            Player.update (world & playerActions) rndGen dt player
    in world { rndGen = rndGen' }
        & set _player player'
        & _bullets (maybe id (:) bullet)
        . _particles (maybe id (:) particle)

-- | Calculates the closest way to the player (when considering the screenwrap)
--   and updates all enemies to move towards the player.
updateEnemies :: Float -> World -> World
updateEnemies dt world @ World { player = (physics' -> position -> playerPos), .. } =
    world & _enemies (map $ \enemy -> Enemy.update (closestWay enemy) dt enemy)
    where
        closestWay (physics' -> position -> enemyPos) =
            minimumBy (comparing (distance enemyPos)) $ positions screenBounds
        -- All possible positions the player might have:
        positions bounds =
            [ playerPos + (width bounds * w, height bounds * h)
            | w <- [-1, 0, 1]
            , h <- [-1, 0, 1]
            ]

-- | Returns a spawnable object and the world with updated random generator.
spawnObject :: Spawn a => World -> (a, World)
spawnObject world @ World { .. } =
    let
        playerBounds = grow spawnMargins $ player & physics' & bounds
        (x, rndGen') = runState (spawn screenBounds playerBounds) rndGen
    in
        (x, world { rndGen = rndGen' })

-- | Updates the timer for spawning enemies and spawns new enemies.
updateEnemySpawning :: Float -> World -> World
updateEnemySpawning dt world @ World { .. } =
    if enemyTimer <= 0 then
        world
            & spawnEnemy
            & set _enemyTimer spawnTime
    else
        world & _enemyTimer (subtract dt)

-- | Adds a random enemy to the world.
spawnEnemy :: World -> World
spawnEnemy world @ World { .. } =
    let (enemy, world') = spawnObject world
    in world' { enemies = enemy:enemies }

-- | Updates the timer for spawning pickups and spawns new pickups.
updatePickupSpawning :: Float -> World -> World
updatePickupSpawning dt world @ World { .. } =
    if pickupTimer <= 0 && length pickups < 3 then
        world
            & spawnPickup
            & set _pickupTimer spawnTime
    else
        world & _pickupTimer (subtract dt)

-- | Adds a random pickup to the world.
spawnPickup :: World -> World
spawnPickup world @ World { .. } =
    let (pickup, world') = spawnObject world
    in world' { pickups = pickup:pickups }

-- | Updates the bullets and filters expired bullets.
updateBullets :: Float -> World -> World
updateBullets dt world =
    world & _bullets (mapMaybe $ Bullet.update dt)

-- | Updates the particles and filters expired particles.
updateParticles :: Float -> World -> World
updateParticles dt world =
    world & _particles (mapMaybe $ Particle.update dt)

-- | Updates the stars.
updateStars :: Float -> World -> World
updateStars dt world =
    world & _stars (map $ Star.update dt)

-- | Restarts the game and resets the multiplier if the player comes into contact with an enemy
--   and increases the multiplier when the player collides with a pickup.
updatePlayerCollisions :: World -> World
updatePlayerCollisions world @ World { player = player @ (physics' -> position -> pos), .. }
    | any (collides player) enemies =
        initial rndGen
            & _particles (++ explosion pos 0 80 rndGen)
            & set _score score
    | otherwise = case checkCollisions [player] pickups of
        Just (_, _, _, pickups') ->
            world
                { pickups = pickups'
                , multiplier = multiplier + 1
                }
        Nothing ->
            world

-- | Removes an enemy and a bullet and creates an explosion whenever they collide.
updateEnemyCollisions :: World -> World
updateEnemyCollisions world @ World { .. } =
    case checkCollisions enemies bullets of
        Just (enemy, bullet, enemies', bullets') ->
            world
                { enemies = enemies' ++ splitAsteroid enemy
                , bullets = bullets'
                , score = score + value enemy * multiplier
                }
                & _particles (++ explodeBullet bullet enemy)
        Nothing ->
            world
    where
        explodeBullet (physics' -> Physics { .. }) enemy =
            explosion position (velocity * 0.75) (explosionSize enemy) rndGen

-- | Removes a pickup and a bullet whenever they collide.
updatePickupCollisions :: World -> World
updatePickupCollisions world @ World { .. } =
    case checkCollisions pickups bullets of
        Just (_, _, pickups', bullets') ->
            world
                { pickups = pickups'
                , bullets = bullets'
                }
        Nothing ->
            world

-- | Checks collisions between two groups of objects and returns both the colliding objects and the other objects.
checkCollisions :: (HasPhysics a, HasPhysics b) => [a] -> [b] -> Maybe (a, b, [a], [b])
checkCollisions xs ys = go xs ys [] []
    where
        go [] _ _ _ = Nothing
        go (a:as) [] aacc bacc = go as bacc (aacc ++ [a]) []
        go (a:as) (b:bs) aacc bacc
            | collides a b = Just (a, b, aacc ++ as, bacc ++ bs)
            | otherwise = go (a:as) bs aacc (bacc ++ [b])
