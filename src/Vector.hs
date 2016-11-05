{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Vector
    ( Vector, Point
    , module Vector
    ) where

import Control.Monad.State
import System.Random

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import Util

instance Random Vector where
    randomR ((lx, ly), (hx, hy)) =
        runState $ do
            x <- getRandomR (lx, hx)
            y <- getRandomR (ly, hy)
            return (x, y)
    random =
        runState $ do
            x <- getRandom
            y <- getRandom
            return (x, y)

instance Fractional Vector where
    (a, b) / (c, d) = (a / c, b / d)
    fromRational x = tmap fromRational (x, x)

-- | Create a vector from an angle (to the positive Y axis) and a length.
fromAngleLength :: Float -> Float -> Vector
fromAngleLength a l = l `mul` (rotate a unitY)

-- | A vector with the X component set to 1
unitX :: Vector
unitX = (1, 0)

-- | A vector with the Y component set to 1
unitY :: Vector
unitY = (0, 1)

-- | Multiply a vector my a scalar value
mul :: Float -> Vector -> Vector
mul x (a, b) = (x * a, x * b)

-- | Take the dot product of two vectors.
dot :: Vector -> Vector -> Float
dot (a, b) (c, d) = a * c + b * d

-- | Calculate the magnitude of a vector.
magnitude :: Vector -> Float
magnitude = sqrt . magnitude2

-- | Calculate the magnitude squared of a vector. Useful for improving
--   performance.
magnitude2 :: Vector -> Float
magnitude2 v = dot v v

-- | Normalize the vector.
normalize :: Vector -> Vector
normalize v = (1 / magnitude v) `mul` v

-- | Rotate a vector by an angle in radians.
rotate :: Float -> Vector -> Vector
rotate a (x, y) = (x * cos a + y * sin a, y * cos a - x * sin a)

-- | The angle of this vector to the positive Y axis, in radians.
angle :: Vector -> Float
angle (x, y) = (pi / 2) - atan2 y x

-- | The angle of this vector to the positive Y axis, in degrees.
angle' :: Vector -> Float
angle' = (* (180 / pi)) . angle

-- | Calculate the distance between two points.
distance :: Point -> Point -> Float
distance a b = magnitude $ b - a

-- | Calculate the distance between two points squared. Useful for improving
--   performance.
distance2 :: Point -> Point -> Float
distance2 a b = magnitude2 $ b - a
