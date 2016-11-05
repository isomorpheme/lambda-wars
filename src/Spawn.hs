module Spawn where

import Control.Monad.State
import System.Random

import Rectangle

class Spawn a where
    spawn :: RandomGen g => Rectangle -> Rectangle -> State g a
