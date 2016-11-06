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
    -- | Draws the world. Note that most moveable objects are drawn wrapped around the screen.
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

-- | Draws the score of the player.
showScore :: Int -> Rectangle -> Picture
showScore n screen =
    Color white
        $ Translate x y
        $ Scale 0.1 0.1
        $ Text $ "score: " ++ show n
    where
        (x, y) = (left screen, bottom screen) + (0, -15)

-- | Draws the multiplier of the player.
showMultipler :: Int -> Rectangle -> Picture
showMultipler n screen =
    Color white
        $ Translate x y
        $ Scale 0.1 0.1
        $ Text $ "multiplier: x" ++ show n
    where
        (x, y) = (right screen, bottom screen) - (80, 15)

-- | Masks all objects that are drawn outside of the screen.
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

-- | Draws a border around the screen.
border :: Rectangle -> Picture
border = Color white . draw

-- | Draws all objects that can wrap around the screen multiple times, 
--   so you can see them going through the border.
screenWrap :: Rectangle -> Picture -> Picture
screenWrap bounds picture =
    Pictures
        [ Translate (width bounds * w) (height bounds * h) picture
        | w <- [-1, 0, 1]
        , h <- [-1, 0, 1]
        ]
