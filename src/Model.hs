{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import System.Random

import Lens.Micro
import Lens.Micro.TH

import Vector (VectorF, PointF)
import qualified Vector

data RotateAction = NoRotation | RotateLeft | RotateRight
    deriving (Show, Eq)

data MovementAction = NoMovement | Thrust
    deriving (Show, Eq)

data ShootAction = Shoot | DontShoot
    deriving (Show, Eq)

-- | The player's space ship
data Player = Player
    { _position :: PointF
    , _direction :: VectorF
    } deriving Show

makeLenses ''Player

-- updatePosition :: (VectorF -> VectorF) -> Player -> Player
-- updatePosition f player @ Player { position } =
--     player { position = f position }
--
-- updateDirection :: (VectorF -> VectorF) -> Player -> Player
-- updateDirection f player @ Player { direction } =
--     player { direction = f direction }

rotate :: Float -> Player -> Player
-- rotate = updateDirection . Vector.rotate
rotate a = over direction (Vector.rotate a)

translate :: VectorF -> Player -> Player
-- translate = updatePosition . Vector.translate
translate v = over position (Vector.translate v)

-- | The game's state.
data World = World
    { rndGen :: StdGen
      -- ^ The random generator for this world

    , _rotateAction :: RotateAction
      -- ^ The most recent rotation action
    , _movementAction :: MovementAction
      -- ^ The most recent movement action
    , _shootAction :: ShootAction
      -- ^ The most recent shooting action

    , _player :: Player
      -- ^ Information about the player
    } deriving Show

makeLenses ''World

initial :: Int -> World
initial seed = World
    { rndGen = mkStdGen seed
    , _rotateAction = NoRotation
    , _movementAction = NoMovement
    , _shootAction = DontShoot
    , _player = Player Vector.zero Vector.unitY
    }
