module Model.Star where

import Graphics.Gloss

import Draw
import Util
import Vector

data Star = Star
    { position :: Vector
    , depth :: Integer
    } deriving (Show)

defaultStar :: Star
defaultStar = Star 0 3

