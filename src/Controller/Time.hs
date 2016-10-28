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

import Lens.Micro

import Config (rotationSpeed)
import Model

-- | Time handling

handleTime :: Float -> World -> World
handleTime deltaTime world =
    case world & _rotateAction of
        RotateRight ->
            world & player %~ rotate (rotationSpeed * deltaTime)
        RotateLeft ->
            world & player %~ rotate (-rotationSpeed * deltaTime)
        _ ->
            world
