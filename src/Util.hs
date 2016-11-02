{-# LANGUAGE TupleSections #-}

module Util
    ( (&)
    , compose
    , set
    , choose
    , frequency
    , iterateState
    ) where

import Control.Arrow (first)
import Control.Monad.State
import Data.Function ((&))
import System.Random

-- | Compose a list of functions operating on the same type into a single list.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- | Use a setter-like function to set a field of a type to a value.
--   This works pretty much like the 'set' used with lenses, except it's not as
--   general.
set :: ((a -> a) -> b -> b) -> a -> b -> b
set = (. const)

-- These functions are like the ones provided QuickCheck's "Gen" module,
-- but using 'Random' and 'RandomGen' instead of the 'Gen' monad.

-- | Choose a random value from a list.
choose :: (RandomGen g) => [a] -> g -> (a, g)
choose [] = error "Called 'Util.choose' on an empty list"
choose xs = first (xs !!) . randomR (0, length xs - 1)

-- | Choose a random value from a list, given a frequency for each value.
frequency :: RandomGen g => [(Int, a)] -> g -> (a, g)
frequency [] = error "Called 'Util.frequency' on an empty list"
frequency xs = first (`pick` xs) . randomR (0, total)
    where
        total = sum $ map fst xs
        pick n ((w, y):ys)
            | n <= w = y
            | otherwise = pick (n - w) xs

-- | Infinitely apply a stateful function, chaining the result states.
iterateState :: (g -> (a, g)) -> g -> [a]
iterateState = evalState . sequence . fmap state . repeat
