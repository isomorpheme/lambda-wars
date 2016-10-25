{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model where

import System.Random

-- | The game's state.
data World = World
    { rndGen           :: StdGen
      -- ^ The random generator for this world

    , rotateAction     :: RotateAction
      -- ^ The most recent rotation action
    , movementAction   :: MovementAction
      -- ^ The most recent movement action
    , shootAction      :: ShootAction
      -- TODO: add more fields here!
    }

data RotateAction = NoRotation | RotateLeft | RotateRight

data MovementAction = NoMovement | Thrust

data ShootAction = Shoot | DontShoot

initial :: Int -> World
initial seed = error "implement Model.initial!"
