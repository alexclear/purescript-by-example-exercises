module PBE.ArrayComprehensionsGuards where

import Prelude

import Control.MonadZero (guard)
import Data.Array (index, (!!), length, null, (..))

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian a b = do
  i <- a
  j <- b
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- a .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]
