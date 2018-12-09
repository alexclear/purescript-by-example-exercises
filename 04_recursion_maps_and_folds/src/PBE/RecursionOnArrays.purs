module PBE.RecursionOnArrays where

import Prelude
import Data.Array (null, filter)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not isEven (n - 1)

numEvens :: Array Int -> Int
numEvens arr =
  if null arr
    then 0
    else if isEven (unsafePartial head arr)
      then 1 + numEvens (unsafePartial tail arr)
      else numEvens (unsafePartial tail arr)

squares :: Array Number -> Array Number
squares = map (\n -> n * n)

filterNegatives :: Array Int -> Array Int
filterNegatives = filter (\n -> n >= 0)
