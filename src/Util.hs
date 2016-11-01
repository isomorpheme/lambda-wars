module Util
    ( (&)
    , compose
    , set
    ) where

import Data.Function ((&))

-- | Compose a list of functions operating on the same type into a single list.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- | Use a setter-like function to set a field of a type to a value.
--   This works pretty much like the 'set' used with lenses, except it's not as
--   general.
set :: ((a -> a) -> b -> b) -> a -> b -> b
set = (. const)
