{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model.World where

import System.Random

import Graphics.Gloss

import Config
import Draw
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

instance Draw World where
    draw World { .. } =
        Pictures $
            map drawWrapped
            [ draw player
            , draw enemies
            , draw bullets
            ]
            ++
            [ draw stars
            , draw particles
            , drawBorder
            , drawCamera
            , drawHearts 5
            ]

drawHeart :: Picture
drawHeart = Color white $ draw $ rectangle 0 6 12

drawHearts :: Integer -> Picture
drawHearts n = Pictures [Translate (x + fromInteger dx * 8) y drawHeart | dx <- [0..n]]
    where
        x = 0 - cameraWidth / 2 + 5
        y = 0 - cameraHeight / 2 - 10

drawBorder :: Picture
drawBorder =
    Pictures
        [ Polygon [(w, h), (4.5 * w, 4.5 * h), (4.5 * w, -4.5 * h), (w, -h)]
        , Polygon [(w, h), (4.5 * w, 4.5 * h), (-4.5 * w, 4.5 * h), (-w, h)]
        , Polygon [(-w, -h), (-4.5 * w, -4.5 * h), (-4.5 * w, 4.5 * h), (-w, h)]
        , Polygon [(-w, -h), (-4.5 * w, -4.5 * h), (4.5 * w, -4.5 * h), (w, -h)]
        ]
            where
                w = cameraWidth / 2
                h = cameraHeight / 2


drawCamera :: Picture
drawCamera =
    Color white $ Line
        [ (-w, h)
        , (w, h)
        , (w, -h)
        , (-w, -h)
        , (-w, h)
        ]
            where
                w = cameraWidth / 2
                h = cameraHeight / 2

drawWrapped :: Picture -> Picture
drawWrapped picture =
    Pictures
        [ Translate (cameraWidth * w) (cameraHeight * h) picture
        | w <- [-1, 0, 1]
        , h <- [-1, 0, 1]
        ]
