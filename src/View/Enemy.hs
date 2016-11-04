{-# LANGUAGE ViewPatterns #-}

module View.Enemy where

import Graphics.Gloss

import Draw
import Model.Enemy
import Physics

instance Draw EnemyType where
    draw (Asteroid size rotation) =
        Rotate rotation
            $ Scale size size
            $ lineLoop
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
            $ lineLoop
                [ (-2, 2)
                , (0, 1)
                , (2, 2)
                , (1, 0)
                , (2, -2)
                , (0, -1)
                , (-2, -2)
                , (-1, 0)
                ]

instance Draw Enemy where
    draw Enemy { physics = position -> (x, y), enemyType = enemyType } =
        Color white
            $ Translate x y
            $ draw enemyType
