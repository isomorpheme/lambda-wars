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
import Model.Player
import Physics
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

    , rotateAction :: RotateAction
      -- ^ The most recent rotation action
    , movementAction :: MovementAction
      -- ^ The most recent movement action
    , shootAction :: ShootAction
      -- ^ The most recent shooting action

    , player :: Player
      -- ^ Information about the player
    , enemies :: [Enemy]
      -- ^ All the enemies
    , bullets :: [Bullet]
      -- ^ All the bullets
    } deriving Show

initial :: Int -> World
initial seed = World
    { rndGen = mkStdGen seed
    , rotateAction = NoRotation
    , movementAction = NoMovement
    , shootAction = DontShoot
    , player = defaultPlayer
    , enemies =
        -- TODO: spawn enemies over time
        take 10
            $ iterateState (spawn (Vector (-100) (-100), Vector 100 100) undefined)
            $ mkStdGen seed
    , bullets = []
    }

_player :: (Player -> Player) -> World -> World
_player f world @ World { player } =
    world { player = f player }

_bullets :: ([Bullet] -> [Bullet]) -> World -> World
_bullets f world @ World { bullets } =
    world { bullets = f bullets }

_enemies :: ([Enemy] -> [Enemy]) -> World -> World
_enemies f world @ World { enemies} =
    world { enemies = f enemies }

playerActions :: World -> PlayerActions
playerActions World { movementAction, rotateAction, shootAction } =
    (movementAction, rotateAction, shootAction)

stepPhysics :: Float -> World -> World
stepPhysics dt =
    compose
        [ _player . _physics $ step dt
        , _bullets . _physics $ step dt
        , _enemies . _physics $ step dt
        ]

instance Draw World where
    draw World { .. } =
        Pictures $
            map drawWrapped
            [ draw player
            , draw enemies
            , draw bullets
            ]
            ++
            [ drawBorder
            , drawCamera
            ]

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