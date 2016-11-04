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
                [ draw player
                , draw enemies
                , draw bullets
                ]
            rest =
                [ draw stars
                , draw particles
                , mask screenBounds
                , border screenBounds
                , lives 5 $ dimensions screenBounds / (-2) + (5, -10)
                ]

lives :: Integer -> Point -> Picture
lives n (x, y) =
    Pictures
        [Translate (x + dx * 8) y life | dx <- [0 .. fromInteger n]]
    where
        life = Color white $ draw $ rectangle 0 6 12

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
