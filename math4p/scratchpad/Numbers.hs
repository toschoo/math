module Numbers
where

  import Data.List (nub, sortBy)
  import Debug.Trace (trace)

  data Digit  = Zero | One | Two   | Three | Four | 
                Five | Six | Seven | Eight | Nine
    deriving (Eq,Ord)

  instance Show Digit where
    show Zero  = "0"
    show One   = "1"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"

  type Number = [Digit]

  unity :: Number
  unity = [One]

  zero :: Number
  zero = [Zero]

  next :: Number -> Number
  next []        = []
  next [Zero]    = [One]
  next [One]     = [Two]
  next [Two]     = [Three]
  next [Three]   = [Four]
  next [Four]    = [Five]
  next [Five]    = [Six]
  next [Six]     = [Seven]
  next [Seven]   = [Eight]
  next [Eight]   = [Nine]
  next [Nine]    = [One,Zero]
  next (Zero:ds) = next ds
  next ds        = case next [last ds] of
                     [_,h2] -> next (init ds) ++ [h2] 
                     [h1]   ->       init ds  ++ [h1]

  prev :: Number -> Number
  prev []        = []
  prev [Zero]    = undefined
  prev [One]     = [Zero]
  prev [Two]     = [One]
  prev [Three]   = [Two]
  prev [Four]    = [Three]
  prev [Five]    = [Four]
  prev [Six]     = [Five]
  prev [Seven]   = [Six]
  prev [Eight]   = [Seven]
  prev [Nine]    = [Eight]
  prev (Zero:ds) = prev ds
  prev ds        = case last ds of
                     Zero -> clean $ prev (init ds) ++ [Nine]
                     h    -> clean $       init ds  ++ prev [h]

  clean :: Number -> Number
  clean [Zero]     = [Zero]
  clean (Zero:ds)  = clean ds
  clean ds         = ds

  cmp :: Number -> Number -> Ordering
  cmp a b | len a > len b = GT
          | len a < len b = LT
          | otherwise = go a b
    where go [] [] = EQ
          go _  [] = GT
          go [] _  = LT
          go (a:as) (b:bs) | a > b = GT
                           | a < b = LT
                           | otherwise = go as bs

  add :: Number -> Number -> Number
  add a [Zero] = a
  add [Zero] b = b
  add a      b = next a `add` (prev b)

  add2 :: Number -> Number -> Number
  add2 as bs     = reverse $ go (reverse as) (reverse bs)
    where  go [] ys          = ys
           go xs []          = xs
           go (x:xs) (y:ys)  = case  add [x] [y] of
                                     [_,r]  -> r : go xs (go ys [One])
                                     [r]    -> r : go xs ys
                                     _      -> undefined

  mul :: Number -> Number -> Number 
  mul _ [Zero] = [Zero]
  mul [Zero] _ = [Zero]
  mul a [One]  = a
  mul [One] b  = b
  mul a     b  = a `add` (a `mul` (prev b))

  mul3 :: Number -> Number -> Number -- [(Int,Number)]  
  mul3 a b = let x = topoly a
                 y = topoly b
              in frompoly $ merge $  simplify $ sortBy lt $ mulNN x y
    where part = map (:[])
          topoly x = let es = enum [Zero] (len x) 
                      in zip es (reverse $ part x)
          frompoly = concat . reverse . map snd
          gt (x1,_) (x2,_) = compare x2 x1        
          lt (x1,_) (x2,_) = compare x1 x2
          mul1 (x1,n1) (x2,n2) = (x1 `add` x2,mul n1 n2)
          mulN n ys = map (mul1 n) ys
          mulNN xs ys = concat $ map (\x -> mulN x ys) xs
          simplify [] = []
          simplify [x] = [x]
          simplify ((x1,n1):(x2,n2):ys) 
            | x1 == x2  = simplify ((x1,add n1 n2):ys)
            | otherwise = (x1,n1):simplify ((x2,n2):ys)
          merge [] = []
          merge [x] = [x]
          merge ((x1,n1):(x2,n2):ys) 
            | x1 == x2 `sub` unity &&
              len n1 > unity = (x1,[last n1]):merge ((x2,add (init n1) n2):ys)
            | otherwise      = (x1,n1):merge ((x2,n2):ys)


  mul2 :: Number -> Number -> Number
  mul2 a b = mulN a (trail (len a) ++ trail (len b))
    where  mulN [] _            = zero
           mulN (Zero:xs) zs    = mulN xs (tail zs)
           mulN (x:xs)    zs    = add2  (mul1 [x] b zs) 
                                        (mulN xs $ tail zs) 
           mul1 _ []  _         = zero
           mul1 x (Zero:ys) zs  = mul1 x ys (tail zs) 
           mul1 x (y:ys)    zs  =  add2  ((x `mul` [y]) ++ zs)
                                         (mul1 x ys $ tail zs)
           trail [Zero]         = []
           trail [One]          = []
           trail n              = Zero : trail (prev n)

  -- mul1 :: Digit -> Number -> Number
  

  enum :: Number -> Number -> [Number]
  enum l u | l > u  = []
           | otherwise = go l u []
    where go a b ns | a == b = (a:ns) 
                    | otherwise = go a (b `sub` unity) (b:ns)

  len :: [a] -> Number
  len = go [Zero]
    where go l [] = l
          go l (x:xs) = go (next l) xs

  sub :: Number -> Number -> Number
  sub a [Zero] = a
  sub a b      = prev a `sub` (prev b)

  isZero :: Number -> Bool
  isZero n = null n || nub n == [Zero]
           
  nQuotRem :: Number -> Number -> (Number,Number)
  nQuotRem _ [Zero] = error "division by zero"
  nQuotRem [Zero] _ = ([Zero],[Zero])
  nQuotRem a b      = go a b [Zero]
    where go x y q | x `cmp` y == LT = (q, x)
                   | otherwise =  go (x `sub` y) y (next q)

  nQuotRem2 :: Number -> Number -> (Number,Number)
  nQuotRem2 _ [Zero] = error "division by zero"
  nQuotRem2 [Zero] _ = ([Zero],[Zero])
  nQuotRem2 a b  = go zero a -- | len a == len b = nQuotRem a b
                             -- | otherwise      = go zero a
    where go q r | r `cmp` b == LT = (q,r)
                 | otherwise =
            let l = len b `add` unity
                x = clean $ nTake l r 
                y = nDrop l r
                (q',r') = if isZero x then (zero,zero) else nQuotRem x b
             in if not (null y) && (r' ++ y) `cmp` b == LT -- last step if r < b: add zero!
                  then (clean $ q ++ q' ++ toZero y, 
                        clean $ r'++ y) 
                  else go (clean $ q ++ q') (r' ++ y)
          toZero = map (\_ -> Zero)

  nGcd :: Number -> Number -> Number
  nGcd a [Zero] = a
  nGcd [Zero] b = b
  nGcd a      b = nGcd b (a `nRem` b)

  nRem :: Number -> Number -> Number
  nRem a = snd . nQuotRem a

  divisable :: Number -> Number -> Bool
  divisable a b = isZero $ snd (a `nQuotRem2` b)

  nTake, nDrop :: Number -> [a] -> [a]
  nTake _ [] = []
  nTake [Zero] xs = []
  nTake n (x:xs) = x : nTake (n `sub` unity) xs

  nDrop _ [] = []
  nDrop [Zero] xs = xs
  nDrop n (x:xs) = nDrop (n `sub` unity) xs 

  fromInt :: Integer -> Number
  fromInt = clean . conv . show
    where conv []  = []
          conv "0" = [Zero]
          conv "1" = [One]
          conv "2" = [Two]
          conv "3" = [Three]
          conv "4" = [Four]
          conv "5" = [Five]
          conv "6" = [Six]
          conv "7" = [Seven]
          conv "8" = [Eight]
          conv "9" = [Nine]
          conv (i:is) = conv [i] ++ conv is

  toInt :: Number -> Integer
  toInt []      = error "nothing is not null"
  toInt [Zero]  = 0
  toInt [One]   = 1
  toInt [Two]   = 2
  toInt [Three] = 3
  toInt [Four]  = 4
  toInt [Five]  = 5
  toInt [Six]   = 6
  toInt [Seven] = 7
  toInt [Eight] = 8
  toInt [Nine]  = 9
  toInt (d:ds)  = let k = length ds 
                   in 10^k * (toInt [d]) + toInt ds

  root2 :: Integer -> (Integer,Integer)
  root2 n = let s = fromIntegral (length $ show n)
                k = if s > 3 then 10^(s `div` 2 + 1)
                             else n 
             in reduce k k 1
    where reduce p x f | x * x == n = (x,0)
                       | x * x > n = reduce p (p `div` f) (f * 2)
                       | x * x < n = ply x 2
          ply x f | x * x == n = (x,0)
                  | x * x > n  = case x `div` f of
                                   0  -> ply (x - 1) f
                                   x' -> ply (x - x') (f+1)
                  | (x+1) * (x+1) > n = (x,n-x*x)
                  | otherwise  = case x `div` f of
                                   0  -> ply (x + 1) f
                                   x' -> ply (x + x') (f+1)

    {-
    where go p x f | x * x == n = (x,0)
                   | x * x > n = case p `div` f of
                                   0  -> {- trace (show x) $ -} (x,n-x*x)
                                   x' -> {- trace (show x) $ -} go p x' (f+1)
                   | x * x < n = (x,n - x * x)
    -}
    
    {-
    where go p x f d | x * x == n = (x,0)
                     | x * x > n = let f' = if d == 0 then f * 2 
                                                      else f + 1
                                    in {- trace ("x: " ++ show x ++ "/" ++
                                              "f: " ++ show f') $-} go p (p `div` f') f' 0
                     | x * x < n && (x+1) * (x+1) > n = (x,n - x * x)
                     | f == 1 = (x,n-x*x)
                     | x * x < n = let f' = f - 1 -- if d == 1 then f `div` 2
                                                  --    else f - 1 
                                    in {- trace ("x: " ++ show x ++ "/" ++
                                              "f: " ++ show f') $ -} go p (p `div` f') f' 1
    -}
    {-
    where go x | x * x == n = (x,0)
               | x * x > n = case x `div` 2 of
                                 0  -> trace (show x) $ go (x - 1)
                                 x' -> trace (show x) $ go x'
               | x * x < n && (x+1) * (x+1) > n = (x,n - x * x)
               | x * x < n = case x `div` 3 of
                                 0  -> trace (show x) $ go (x + 1)
                                 x' -> trace (show x) $ go (x + x')
    -}
    {-
    where go p x f d | x * x == n = (x,0)
                     | x * x > n = let f' = if d == 0 then f * 2 
                                                      else f + 1
                                    in {- trace ("x: " ++ show x ++ "/" ++
                                              "f: " ++ show f') $-} go p (p `div` f') f' 0
                     | x * x < n && (x+1) * (x+1) > n = (x,n - x * x)
                     | f == 1 = (x,n-x*x)
                     | x * x < n = let f' = f - 1 -- if d == 1 then f `div` 2
                                                  --    else f - 1 
                                    in {- trace ("x: " ++ show x ++ "/" ++
                                              "f: " ++ show f') $ -} go p (p `div` f') f' 1
    -}

  root2_ :: Integer -> (Integer,Integer)
  root2_ n = go (n `div` 2) 2 
    where go x f | x * x == n = (x,0)
                 | x * x > n = let f' = f * 2 in go (n `div` f') f' 
                 | x * x < n && (x+1) * (x+1) > n = (x,n - x * x)
                 | x * x < n = let f' = f - 1 in go (n `div` f') f' 

  
