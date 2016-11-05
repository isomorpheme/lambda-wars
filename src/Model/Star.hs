module Model.Star where

import Graphics.Gloss

import Draw
import Util
import Vector

data Star = Star
    { offset :: Vector
    , depth :: Float
    } deriving (Show)

defaultStar :: Star
defaultStar = Star 0 1.8

instance Draw Star where
    draw Star { offset = (x, y), depth } = 
        Color (greyN proximity)
            $ Translate x y
            $ Scale size size
            $ Pictures
                [ Line [(0, -1), (0, 1)]
                , Line [(-1, 0), (1, 0)]
                ]
                where
                    size = 10 * proximity
                    proximity = 1 / depth


