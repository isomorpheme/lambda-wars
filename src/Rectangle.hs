{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rectangle where

import Graphics.Gloss.Data.Picture

import Draw
import Vector
import Util

type Rectangle = (PointF, PointF)

rectangleCorners :: PointF -> PointF -> Rectangle
rectangleCorners = (,)

rectangle :: PointF -> Float -> Float -> Rectangle
rectangle center width height =
    let offset = (/ 2) <$> Vector width (-height)
    in (center `sub` offset, center `add` offset)

square :: PointF -> Float -> Rectangle
square center size = rectangle center size size

width :: Rectangle -> Float
width (Vector l _, Vector r _ ) = r - l

height :: Rectangle -> Float
height (Vector _ t, Vector _ b) = t - b

top :: Rectangle -> Float
top (Vector _ y, _) = y

bottom :: Rectangle -> Float
bottom (_, Vector _ y) = y

right :: Rectangle -> Float
right (_, Vector x _) = x

left :: Rectangle -> Float
left (Vector x _, _) = x

corners :: Rectangle -> [PointF]
corners (Vector l t, Vector r b) =
    [Vector l t, Vector r t, Vector r b, Vector l b]

contains :: PointF -> Rectangle -> Bool
contains (Vector x y) rectangle =
    let [t, b, r, l] = map ($ rectangle) [top, bottom, right, left]
    in and [t > y, y > b, r > x, x > l]

intersects :: Rectangle -> Rectangle -> Bool
intersects a b =
    any (`contains` b) $ corners a

instance Draw Rectangle where
    draw = lineLoop . map toTuple . corners
