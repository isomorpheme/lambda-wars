{-# LANGUAGE ViewPatterns #-}

module Enemy where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Drawable
import Physics
import Vector (Vector(..))
import qualified Vector

data EnemyType
    = Asteroid Float Float
    | Seeker
    deriving (Eq, Show)

data Enemy = Enemy
    { physics :: Physics
    , type' :: EnemyType
    } deriving (Show)

instance Drawable Enemy where
    draw Enemy { physics = position -> Vector x y, type' = type' } =
        Color white
            $ Translate x y 
            $ draw type'

instance Drawable EnemyType where
    
    draw (Asteroid size rotation) = 
        Rotate rotation
            $ Scale size size
            $ Polygon
                [ (-1, 3)
                , (1, 3)
                , (3, 1)
                , (3, -1)
                , (1, -3)
                , (-1, -3)
                , (-3, -1)
                , (-3, 1)
                ]

    draw Seeker =
        Scale 4 4 
            $ Line
                [ (-2, 2)
                , (0, 1)
                , (2, 2)
                , (1, 0)
                , (2, -2)
                , (0, -1)
                , (-2, -2)
                , (-1, 0)
                , (-2, 2)
                ]