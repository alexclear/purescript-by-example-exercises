module PBE.RecursionOnArrays where

import Prelude
import Data.Array (null)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not isEven (n - 1)
