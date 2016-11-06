{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Model.World where

import System.Random

import Config
import Model.Bullet
import Model.Enemy
import Model.Particle
import Model.Pickup
import Model.Player
import Model.Star as Star
import Rectangle
import Util

data RotateAction = NoRotation | RotateLeft | RotateRight
    deriving (Show, Eq)

data MovementAction = NoMovement | Thrust
    deriving (Show, Eq)

data ShootAction = Shoot | DontShoot
    deriving (Show, Eq)

type PlayerActions = (MovementAction, RotateAction, ShootAction)

-- | The game's state.
data World = World
    { rndGen :: StdGen
      -- ^ The random generator for this world
    , screenBounds :: Rectangle
      -- ^ The screen bounds of this world

    , rotateAction :: RotateAction
      -- ^ The most recent rotation action
    , movementAction :: MovementAction
      -- ^ The most recent movement action
    , shootAction :: ShootAction
      -- ^ The most recent shooting action

    , player :: Player
      -- ^ Information about the player
    , score :: Int
      -- ^ The player's score
    , multiplier :: Int
      -- ^ The current multipler

    , bullets :: [Bullet]
      -- ^ All the bullets

    , enemies :: [Enemy]
      -- ^ All the enemies
    , enemyTimer :: Float
      -- ^ Time until another enemy spawns

    , pickups :: [Pickup]
      -- ^ All pickups
    , pickupTimer :: Float
      -- ^ Time until another pickup spawns

    , particles :: [Particle]
      -- ^ All the particles

    , stars :: [Star]
      -- ^ All the stars
    } deriving Show

-- | The initial state of the world.
initial :: StdGen -> World
initial rndGen = World
    { rndGen
    , screenBounds = screenBounds
    , rotateAction = NoRotation
    , movementAction = NoMovement
    , shootAction = DontShoot
    , player = defaultPlayer
    , score = 0
    , multiplier = 1
    , bullets = []
    , enemies = []
    , enemyTimer = spawnTime
    , pickups = []
    , pickupTimer = spawnTime
    , particles = []
    , stars = stars
    }
    where
        stars = take 100 $ iterateState (Star.spawn screenBounds) rndGen
        screenBounds = rectangle 0 (cameraWidth, cameraHeight)

-- | Changes the player.
_player :: (Player -> Player) -> World -> World
_player f world @ World { player } =
    world { player = f player }

-- | Changes the score.
_score :: (Int -> Int) -> World -> World
_score f world @ World { score } =
    world { score = f score }

-- | Changes the bullets.
_bullets :: ([Bullet] -> [Bullet]) -> World -> World
_bullets f world @ World { bullets } =
    world { bullets = f bullets }

-- | Changes the enemies.
_enemies :: ([Enemy] -> [Enemy]) -> World -> World
_enemies f world @ World { enemies } =
    world { enemies = f enemies }

-- | Changes the pickups.
_pickups :: ([Pickup] -> [Pickup]) -> World -> World
_pickups f world @ World { pickups } =
    world { pickups = f pickups }

-- | Changes the enemyTimer.
_enemyTimer :: (Float -> Float) -> World -> World
_enemyTimer f world @ World { enemyTimer } =
    world { enemyTimer = f enemyTimer }

-- | Changes the pickupTimer.
_pickupTimer :: (Float -> Float) -> World -> World
_pickupTimer f world @ World { pickupTimer } =
    world { pickupTimer = f pickupTimer }

-- | Changes the particles.
_particles :: ([Particle] -> [Particle]) -> World -> World
_particles f world @ World { particles } =
    world { particles = f particles }

-- | Changes the stars.
_stars :: ([Star] -> [Star]) -> World -> World
_stars f world @ World { stars } =
    world { stars = f stars }

-- | Returns all the playerActions.
playerActions :: World -> PlayerActions
playerActions World { movementAction, rotateAction, shootAction } =
    (movementAction, rotateAction, shootAction)
