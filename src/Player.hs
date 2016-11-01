{-# LANGUAGE ViewPatterns #-}

module Player where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics
import Vector (Vector(..))
import qualified Vector

-- | The player's space ship
data Player = Player
    { physics :: Physics
    , direction :: Float
    } deriving Show

defaultPlayer :: Player
defaultPlayer = Player initialPhysics 0

updatePhysics :: (Physics -> Physics) -> Player -> Player
updatePhysics f player @ Player { physics } =
    player { physics = f physics }

updateDirection :: (Float -> Float) -> Player -> Player
updateDirection f player @ Player { direction } =
    player { direction = f direction }

rotate :: Float -> Player -> Player
rotate = updateDirection . (+)

instance Draw Player where
    draw Player { physics = position -> Vector x y, direction } =
        Color white
            $ Translate x y
            $ Rotate (radToDeg direction) ship
        where
            ship = Scale 3 3 $ Line
                [ (0, 4)
                , (2, -2)
                , (0, -1)
                , (-2, -2)
                , (0, 4)
                ]
