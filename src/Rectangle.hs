module Rectangle where

import Graphics.Gloss.Data.Picture

import Draw
import Vector
import Util

data Rectangle = Rectangle
    { topLeft :: PointF
    , bottomRight :: PointF
    }

rectangleCorners :: PointF -> PointF -> Rectangle
rectangleCorners = Rectangle

rectangle :: PointF -> Float -> Float -> Rectangle
rectangle center width height =
    let offset = (/ 2) <$> Vector width (-height)
    in Rectangle
        { topLeft = center `sub` offset
        , bottomRight = center `add` offset
        }

square :: PointF -> Float -> Rectangle
square center size = rectangle center size size

width :: Rectangle -> Float
width Rectangle { topLeft = Vector l _, bottomRight = Vector r _ } =
    r - l

height :: Rectangle -> Float
height Rectangle { topLeft = Vector _ t, bottomRight = Vector _ b } =
    t - b

top :: Rectangle -> Float
top Rectangle { topLeft = Vector _ y } = y

bottom :: Rectangle -> Float
bottom Rectangle { bottomRight = Vector _ y } = y

right :: Rectangle -> Float
right Rectangle { bottomRight = Vector x _ } = x

left :: Rectangle -> Float
left Rectangle { topLeft = Vector x _ } = x

corners :: Rectangle -> [PointF]
corners Rectangle { topLeft = Vector l t, bottomRight = Vector r b } =
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
