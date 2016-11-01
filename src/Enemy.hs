{-# LANGUAGE ViewPatterns #-}

module Enemy where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Drawable
import Physics
import Vector (Vector(..))
import qualified Vector

data EnemyType
    = Seeker
    deriving (Eq, Show)

data Enemy = Enemy
    { physics :: Physics
    , type' :: EnemyType
    } deriving (Show)

instance Drawable Enemy where
    draw Enemy { physics = position -> Vector x y } =
        Color white
            $ Translate x y asteroid
        where
            asteroid = Scale 3 3 $ Polygon
                [ (-1, 3)
                , (1, 3)
                , (3, 1)
                , (3, -1)
                , (1, -3)
                , (-1, -3)
                , (-3, -1)
                , (-3, 1)
                ]
