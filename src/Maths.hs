module Maths
  ( roundHalfUp,
  )
where

-- number -> expected decimal digits -> result
roundHalfUp :: Float -> Int -> Float
roundHalfUp n d = fromInteger (floor $ n * multiplier + 0.5) / multiplier
  where
    multiplier = 10 ^ d