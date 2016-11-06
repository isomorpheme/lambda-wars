{-# LANGUAGE RecordWildCards #-}

module View.World where

import Graphics.Gloss

import Draw
import Model.World
import Rectangle
import Util
import View.Enemy
import View.Player

instance Draw World where
    draw World { .. } =
        Pictures $ wrapped ++ rest
        where
            wrapped = map (screenWrap screenBounds)
                [ draw stars
                , draw player
                , draw enemies
                , draw bullets
                , draw pickups
                ]
            rest =
                [ draw particles
                , mask screenBounds
                , border screenBounds
                , showScore score screenBounds
                , showMultipler multiplier screenBounds
                ]

showLives :: Int -> Rectangle -> Picture
showLives n screen =
    Pictures
        [Translate (x + dx * 8) y life | dx <- [0 .. fromIntegral n]]
    where
        life = Color white $ draw $ rectangle 0 (6, 12)
        (x, y) = dimensions screen / (-2) + (5, -10)

showScore :: Int -> Rectangle -> Picture
showScore n screen =
    Color white
        $ Translate x y
        $ Scale 0.1 0.1
        $ Text $ "score: " ++ show n
    where
        (x, y) = (left screen, bottom screen) + (0, -15)

showMultipler :: Int -> Rectangle -> Picture
showMultipler n screen =
    Color white
        $ Translate x y
        $ Scale 0.1 0.1
        $ Text $ "multiplier: x" ++ show n
    where
        (x, y) = (right screen, bottom screen) - (80, 15)

mask :: Rectangle -> Picture
mask area =
    Pictures
        [ Polygon [(w, h), (4.5 * w, 4.5 * h), (4.5 * w, -4.5 * h), (w, -h)]
        , Polygon [(w, h), (4.5 * w, 4.5 * h), (-4.5 * w, 4.5 * h), (-w, h)]
        , Polygon [(-w, -h), (-4.5 * w, -4.5 * h), (-4.5 * w, 4.5 * h), (-w, h)]
        , Polygon [(-w, -h), (-4.5 * w, -4.5 * h), (4.5 * w, -4.5 * h), (w, -h)]
        ]
    where
        (w, h) = tmap (/ 2) $ dimensions area

border :: Rectangle -> Picture
border = Color white . draw

screenWrap :: Rectangle -> Picture -> Picture
screenWrap bounds picture =
    Pictures
        [ Translate (width bounds * w) (height bounds * h) picture
        | w <- [-1, 0, 1]
        , h <- [-1, 0, 1]
        ]
