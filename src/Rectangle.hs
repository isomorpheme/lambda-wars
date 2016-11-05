{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rectangle where

import Control.Arrow ((&&&))
import Control.Monad.State
import System.Random

import Graphics.Gloss.Data.Picture

import Draw
import Vector
import Util

type Rectangle = (Point, Point)

rectangleCorners :: Point -> Point -> Rectangle
rectangleCorners = (,)

rectangle :: Point -> Float -> Float -> Rectangle
rectangle center width height =
    let offset = tmap (/ 2) (width, -height)
    in (center - offset, center + offset)

square :: Point -> Float -> Rectangle
square center size = rectangle center size size

width :: Rectangle -> Float
width ((l, _), (r, _) ) = abs $ r - l

height :: Rectangle -> Float
height ((_, t), (_, b)) = abs $ t - b

dimensions :: Rectangle -> (Float, Float)
dimensions = width &&& height

center :: Rectangle -> Point
center (topLeft, bottomRight) = topLeft + (bottomRight - topLeft) / 2

top :: Rectangle -> Float
top ((_, y), _) = y

bottom :: Rectangle -> Float
bottom (_, (_, y)) = y

right :: Rectangle -> Float
right (_, (x, _)) = x

left :: Rectangle -> Float
left ((x, _), _) = x

corners :: Rectangle -> [Point]
corners ((l, t), (r, b)) =
    [(l, t), (r, t), (r, b), (l, b)]

contains :: Rectangle -> Point -> Bool
contains rect (x, y) =
    let [t, b, r, l] = map ($ rect) [top, bottom, right, left]
    in and [t > y, y > b, r > x, x > l]

intersects :: Rectangle -> Rectangle -> Bool
intersects a b =
    not $ or
        [ right a < left b
        , left a > right b
        , top a < bottom b
        , bottom a > top b
        ]

grow :: (Float, Float) -> Rectangle -> Rectangle
grow amount rect = rectangle (center rect) w h
    where (w, h) = dimensions rect + amount

instance Draw Rectangle where
    draw = lineLoop . corners

wrap :: Rectangle -> Vector -> Vector
wrap area (x, y) =
    (wrap' (width area) x, wrap' (height area) y)
    where
        wrap' size value
            | value < 0 - size / 2 = value + size
            | value > size / 2 = value - size
            | otherwise = value

randomAvoid :: RandomGen g => Rectangle -> Rectangle -> State g Vector
randomAvoid bounds avoid = do
    position <- getRandomR bounds
    if avoid `contains` position then
        randomAvoid bounds avoid
    else
        return position
