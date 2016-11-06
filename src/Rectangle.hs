{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- See the comment in Vector.hs
{-# OPTIONS -fno-warn-orphans #-}

module Rectangle where

import Control.Arrow ((&&&))
import Control.Monad.State
import System.Random

import Graphics.Gloss.Data.Picture

import Draw
import Util
import Vector

-- | An axis aligned rectangle, consisting of top left and bottom right corners.
type Rectangle = (Point, Point)

instance Draw Rectangle where
    draw = lineLoop . corners

-- | Construct a rectangle from a center position, width and height.
rectangle :: Point -> (Float, Float) -> Rectangle
rectangle center (width, height) =
    let offset = tmap (/ 2) (width, -height)
    in (center - offset, center + offset)

-- | Construct a square rectangle.
square :: Point -> Float -> Rectangle
square center size = rectangle center (size, size)

-- | Calculate the width of a rectangle.
width :: Rectangle -> Float
width ((l, _), (r, _) ) = abs $ r - l

-- | Calculate the height of a rectangle.
height :: Rectangle -> Float
height ((_, t), (_, b)) = abs $ t - b

-- | Calculate the width and height of a rectangle.
dimensions :: Rectangle -> (Float, Float)
dimensions = width &&& height

-- | Calculate the center of a rectangle.
center :: Rectangle -> Point
center (topLeft, bottomRight) = topLeft + (bottomRight - topLeft) / 2

-- | Get the Y coordinate of the top bound of a rectangle.
top :: Rectangle -> Float
top ((_, y), _) = y

-- | Get the Y coordinate of the bottom bound of a rectangle.
bottom :: Rectangle -> Float
bottom (_, (_, y)) = y

-- | Get the X coordinate of the right bound of a rectangle.
right :: Rectangle -> Float
right (_, (x, _)) = x

-- | Get the X coordinate of the left bound of a rectangle.
left :: Rectangle -> Float
left ((x, _), _) = x

-- | Get all the corners of a rectangle.
corners :: Rectangle -> [Point]
corners ((l, t), (r, b)) =
    [(l, t), (r, t), (r, b), (l, b)]

-- | Check whether a rectangle contains a position.
contains :: Rectangle -> Point -> Bool
contains rect (x, y) =
    let [t, b, r, l] = map ($ rect) [top, bottom, right, left]
    in and [t > y, y > b, r > x, x > l]

-- | Check whether two rectangles intersect.
intersects :: Rectangle -> Rectangle -> Bool
intersects a b =
    not $ or
        [ right a < left b
        , left a > right b
        , top a < bottom b
        , bottom a > top b
        ]

-- | Expand a rectangle by an amount, outward from the center.
grow :: (Float, Float) -> Rectangle -> Rectangle
grow amount rect = rectangle (center rect) $ dimensions rect + amount

-- | Wrap a position inside a rectangle.
wrap :: Rectangle -> Vector -> Vector
wrap area (x, y) =
    (wrap' (width area) x, wrap' (height area) y)
    where
        wrap' size value
            | value < -size / 2 = value + size
            | value > size / 2 = value - size
            | otherwise = value

-- | Randomly generate a vector that is *not* inside a rectangle.
randomAvoid :: RandomGen g => Rectangle -> Rectangle -> (Float, Float) -> State g Vector
randomAvoid bounds avoid margins = do
    position <- getRandomR bounds
    if avoid `intersects` rectangle position margins then
        randomAvoid bounds avoid margins
    else
        return position
