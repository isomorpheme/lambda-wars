{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    handleTime
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss

import System.Random

import Model

-- | Time handling

handleTime :: Float -> World -> World
handleTime time = error "implement Controller.Time.timeHandler!"
