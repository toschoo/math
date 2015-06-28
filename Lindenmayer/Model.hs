module Lindenmayer.Model where

  import Lindenmayer.Common 

  ----------------------------------------------------------------------------
  -- A Model is defined by a function that turns
  --         - a string (F+F--F+F)
  --         - and a point (from where we start)
  --         - and a Parameter (length, angle)
  --         - into a list of lines
  ----------------------------------------------------------------------------
  type Model = (String -> Point -> Param -> [Line])

  forward :: Char
  forward = 'F'
  
  rightTurn :: Char
  rightTurn = '+'

  leftTurn :: Char
  leftTurn = '-'

  ----------------------------------------------------------------------------
  -- Get Model
  ----------------------------------------------------------------------------
  getModel :: String -> Maybe Model
  getModel "koch" = Just koch
  getModel _      = Nothing

  ----------------------------------------------------------------------------
  -- Koch Curve
  ----------------------------------------------------------------------------
  koch :: String -> Point -> Param -> [Line]
  koch [] _ _ = []
  koch (c:cs) p (l, a) = let a' = newAngle c a 
                          in case newPoint c p (l,a) of
                               Nothing -> koch cs p (l, a')
                               Just p' -> (p, p') : koch cs p' (l, a')

  newPoint :: Char -> Point -> Param -> Maybe Point
  newPoint c p (l, a) | c == 'F'  = Just $ pointFromAngle p l a
                      | otherwise = Nothing

  newAngle :: Char -> Int -> Int
  newAngle c a = case c of
                 '+' -> a + 45
                 '-' -> a - 45
                 _   -> a
