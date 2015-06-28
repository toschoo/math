module Sur 
where

  data Sur = Sur [Sur] [Sur]
    deriving (Eq,Show)

  valid :: Sur -> Bool
  valid (Sur l r) = all (cmp l) r

  cmp :: [Sur] -> Sur -> Bool
  cmp [] _ = True
  cmp (x:xs) y | not (y `ge` x) = cmp xs y
               | otherwise      = False

  ge :: Sur -> Sur -> Bool
  ge x y = not (le x y) -- x@(Sur xl xr) y@(Sur yl yr) = 

  le :: Sur -> Sur -> Bool
  le x@(Sur xl xr) y@(Sur yl yr) = cmp xl y && not (cmp yr x)

  zero,one,two,three,four,five,six,seven,eight,nine :: Sur
  zero  = Sur [     ]  []
  one   = Sur [zero ]  []
  two   = Sur [one  ]  []
  three = Sur [two  ]  []
  four  = Sur [three]  []
  five  = Sur [four ]  []
  six   = Sur [five ]  []
  seven = Sur [six  ]  []
  eight = Sur [seven]  []
  nine  = Sur [eight]  []

  natSur :: [Sur]
  natSur = goOn (Sur [] [])

  goOn :: Sur -> [Sur]
  goOn n@(Sur l r) = n : goOn (Sur [n] r)

  natRead :: Sur -> Integer
  natRead (Sur [] [])  = 0
  natRead (Sur [x] []) = natRead x + 1
  natRead _            = error "Not a natural"
  
