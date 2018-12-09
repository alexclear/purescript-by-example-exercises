module PBE.ArrayComprehensionsGuards where

import Prelude

import Control.MonadZero (guard)
import Data.Array (length, null, (..))

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1
