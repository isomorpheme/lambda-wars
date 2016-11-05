{-# LANGUAGE RecordWildCards #-}

module Model.Star where

import Control.Monad.State
import System.Random

import Graphics.Gloss

import Draw
import Rectangle
import Util
import Vector

data Star = Star
    { position :: Vector
    , depth :: Float
    } deriving (Show)

defaultStar :: Star
defaultStar = Star 0 2

_position :: (Point -> Point) -> Star -> Star
_position f star @ Star { position } =
    star { position = f position }

instance Draw Star where
    draw Star { .. } = 
        Color (greyN proximity)
            $ uncurry Translate position
            $ Scale size size
            $ Pictures
                [ Line [(0, -1), (0, 1)]
                , Line [(-1, 0), (1, 0)]
                ]
                where
                    size = 10 * proximity
                    proximity = 1 / depth

spawn :: RandomGen g => Rectangle -> g -> (Star, g)
spawn bounds = runState $ do
    position <- getRandomR bounds
    depth <- getRandomR (2.5, 7.5)
    return $ Star position depth