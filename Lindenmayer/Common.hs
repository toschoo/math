module Lindenmayer.Common where

  type Point = (Int, Int)
  type Line  = (Point, Point)

  type Param = (Int, Int) -- Length, Angle

  type Vocabulary = String
  type Constant   = String
  type Omega      = String
  type Rule       = (Char, String) -- 'F', "F+F--F+F"

  defSize :: Point
  defSize = (1200, 600)

  defStart :: Point
  defStart = (300, 500)

  --------------------------------------------------------------
  -- Given a start point
  --         a length
  --         and an angle
  -- we get a new point
  --------------------------------------------------------------
  pointFromAngle :: Point -> Int -> Int -> (Int, Int)
  pointFromAngle (x, y) l a =
    let l' = fromIntegral l
        a' = fromIntegral a * pi / 180.0 -- toRadiant 
        x' = x + round (l' * (cos a' :: Double))
        y' = y - round (l' * (sin a' :: Double)) -- y is reversed (0 = topmost)
    in (x', y')

