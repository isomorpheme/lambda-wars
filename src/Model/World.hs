{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model.World where

import System.Random

import Graphics.Gloss

import Draw
import Model.Bullet
import Model.Enemy
import Model.Player
import Physics
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
    , enemies = []
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
        Pictures
            [ draw player
            , draw enemies
            , draw bullets
            ]
