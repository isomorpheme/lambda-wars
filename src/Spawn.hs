module Spawn where

import Control.Monad.State
import System.Random

import Rectangle

-- | Anything which can be spawned, given the bounds of the screen and
--   an area to avoid (e.g. around the player).
class Spawn a where
    spawn :: RandomGen g => Rectangle -> Rectangle -> State g a
