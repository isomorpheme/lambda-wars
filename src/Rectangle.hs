{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rectangle where

import Control.Arrow ((&&&))

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
width ((l, _), (r, _) ) = r - l

height :: Rectangle -> Float
height ((_, t), (_, b)) = t - b

dimensions :: Rectangle -> (Float, Float)
dimensions = width &&& height

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
