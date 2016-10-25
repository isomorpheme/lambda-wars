module Vector
    ( -- * The 'Vector' Type
      Vector(..)
      -- ** Basic Arithmetic
    , add, sub, mul, dot
      -- ** Other Functions
    , magnitude, magnitude2, rotate
      -- * The 'Point' Type
    , Point
    , distance, distance2, translate
      -- * Useful Aliases
    , VectorF, PointF
    ) where

-- | A 2D vector, represented as an X and a Y component.
data Vector a = Vector a a
    deriving (Show, Eq)

instance Functor Vector where
    fmap f (Vector x y) = Vector (f x) (f y)

-- | Add two vectors together.
add :: Num a => Vector a -> Vector a -> Vector a
add (Vector a b) (Vector c d) = Vector (a + b) (c + d)

-- | Subtract two vectors.
sub :: Num a => Vector a -> Vector a -> Vector a
sub (Vector a b) (Vector c d) = Vector (a - b) (c - d)

-- | Multiply a vector by a scalar value.
mul :: Num a => a -> Vector a -> Vector a
mul a (Vector x y) = Vector (a * x) (a * y)

-- | Take the dot product of two vectors.
dot :: Num a => Vector a -> Vector a -> a
dot (Vector a b) (Vector c d) = a * c + b * d

-- | Calculate the magnitude of a vector.
magnitude :: Floating a => Vector a -> a
magnitude = sqrt . magnitude2

-- | Calculated the magnitude squared of a vector. Useful for improving
--   performance.
magnitude2 :: Num a => Vector a -> a
magnitude2 v = dot v v

-- | Rotate a vector by an angle in radians.
rotate :: Floating a => a -> Vector a -> Vector a
rotate = undefined

-- | Exactly the same as a normal 'Vector', but with a different meaning:
--   a 'Point' represents a position, as opposed to an offset or direction.
type Point = Vector

-- | Calculate the distance between two points.
distance :: Floating a => Point a -> Point a -> a
distance a b = magnitude (b `sub` a)

-- | Calculate the distance between two points squared. Useful for improving
--   performance.
distance2 :: Num a => Point a -> Point a -> a
distance2 a b = magnitude2 (b `sub` a)

-- | Translate a point with an offset.
--   Exactly the same as 'add'.
translate :: Num a => Point a -> Vector a -> Vector a
translate = add

type VectorF = Vector Float
type PointF = Point Float
