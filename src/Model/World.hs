{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model.World where

import System.Random

import Graphics.Gloss

import Config
import Model.Bullet
import Model.Enemy
import Model.Particle
import Model.Player
import Model.Star
import Physics
import Rectangle
import Util
import Vector

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
    , spawnTimer :: Float
      -- ^ Time until another enemy spawns

    , particles :: [Particle]
      -- ^ All the particles

    , stars :: [Star]
      -- ^ All the stars
    } deriving Show

initial :: StdGen -> World
initial rndGen = World
    { rndGen
    , screenBounds = rectangle 0 cameraWidth cameraHeight
    , rotateAction = NoRotation
    , movementAction = NoMovement
    , shootAction = DontShoot
    , player = defaultPlayer
    , score = 0
    , multiplier = 1
    , bullets = []
    , enemies = []
    , spawnTimer = spawnTime
    , particles = []
    , stars = [defaultStar]
    }

_player :: (Player -> Player) -> World -> World
_player f world @ World { player } =
    world { player = f player }

_bullets :: ([Bullet] -> [Bullet]) -> World -> World
_bullets f world @ World { bullets } =
    world { bullets = f bullets }

_enemies :: ([Enemy] -> [Enemy]) -> World -> World
_enemies f world @ World { enemies } =
    world { enemies = f enemies }

_spawnTimer :: (Float -> Float) -> World -> World
_spawnTimer f world @ World { spawnTimer } =
    world { spawnTimer = f spawnTimer }

_particles :: ([Particle] -> [Particle]) -> World -> World
_particles f world @ World { particles } =
    world { particles = f particles }

playerActions :: World -> PlayerActions
playerActions World { movementAction, rotateAction, shootAction } =
    (movementAction, rotateAction, shootAction)
