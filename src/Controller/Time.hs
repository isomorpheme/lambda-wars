{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Controller.Time
    ( handleTime
    ) where

import Control.Arrow ((>>>))
import Data.List
import System.Random

import Config (rotationSpeed)
import Model

-- | Time handling

handleTime :: Float -> World -> World
handleTime deltaTime world @ World { .. } =
    case rotateAction of
        RotateRight ->
            world { player = rotate (rotationSpeed * deltaTime) player }
        RotateLeft ->
            world { player = rotate (-rotationSpeed * deltaTime) player }
        _ ->
            world
