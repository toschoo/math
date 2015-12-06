module Number
where

  data Digit = Zero | One   | Two   | Three | Four | Five |
               Six  | Seven | Eight | Nine  |
               A | B | C | D | E | F | G | H | I | J | K |
               L | M | N | O | P | Q | R | S | T | U | V | 
               W | X | Y | Z
    deriving (Show,Eq,Ord,Enum)

  d2int :: Digit -> Int
  d2int Zero  = 0
  d2int One   = 1
  d2int Two   = 2
  d2int Three = 3
  d2int Four  = 4
  d2int Five  = 5
  d2int Six   = 6
  d2int Seven = 7
  d2int Eight = 8
  d2int Nine  = 9
  d2int A     = 10
  d2int B     = 11
  d2int C     = 12
  d2int D     = 13
  d2int E     = 14
  d2int F     = 15
  d2int G     = 16
  d2int H     = 17
  d2int I     = 18
  d2int J     = 19
  d2int K     = 20
  d2int L     = 21
  d2int M     = 22
  d2int N     = 23
  d2int O     = 24
  d2int P     = 25
  d2int Q     = 26
  d2int R     = 27
  d2int S     = 28
  d2int T     = 29
  d2int U     = 30
  d2int V     = 31
  d2int W     = 32
  d2int X     = 33
  d2int Y     = 34
  d2int Z     = 35

  int2d :: Int -> Digit
  int2d 0  = Zero
  int2d 1  = One 
  int2d 2  = Two 
  int2d 3  = Three
  int2d 4  = Four
  int2d 5  = Five
  int2d 6  = Six 
  int2d 7  = Seven
  int2d 8  = Eight
  int2d 9  = Nine
  int2d 10 = A
  int2d 11 = B
  int2d 12 = C
  int2d 13 = D
  int2d 14 = E
  int2d 15 = F
  int2d 16 = G
  int2d 17 = H
  int2d 18 = I
  int2d 19 = J
  int2d 20 = K
  int2d 21 = L
  int2d 22 = M
  int2d 23 = N
  int2d 24 = O
  int2d 25 = P
  int2d 26 = Q
  int2d 27 = R
  int2d 28 = S
  int2d 29 = T
  int2d 30 = U
  int2d 31 = V
  int2d 32 = W
  int2d 33 = X
  int2d 34 = Y
  int2d 35 = Z
  int2d _  = undefined
  
  data Number = Number Digit [Digit]
    deriving (Show,Eq)

  instance Enum Number where
    succ (Number _ [])     = undefined
    succ (Number u [Zero]) = Number u [One]
    succ (Number u [d]) | d < u       = Number u [succ d]
                        | d == u      = Number u [One,Zero]
                        | otherwise   = undefined
    succ (Number u ds)  | last ds < u = Number u ((init ds) ++ [succ (last ds)])
                        | otherwise   = case succ (Number u $ init ds) of
                                          Number _ sds -> Number u (sds ++ [Zero])
    pred (Number _ []) = undefined
    pred (Number _ [Zero]) = error "counting beyond Zero!"
    pred (Number u [d])    = Number u [pred d]
    pred (Number u ds)  | last ds > Zero = Number u ((init ds) ++ [pred (last ds)])
                        | otherwise      = case pred (Number u $ init ds) of
                                             Number _ (Zero:pds) -> Number u (pds ++ [u])
                                             Number _ pds        -> Number u (pds ++ [u])
    
    fromEnum    = fromIntegral . n2Integer
    toEnum   i  = error "not yet implemented!"

  n2Integer :: Number -> Integer
  n2Integer (Number u ds) = let b = fromIntegral $ d2int u + 1
                             in go b 0 (reverse ds)
    where go _ _ []        = 0
          go b e (Zero:xs) = go b (e+1) xs
          go b e (x:xs)    = let a = fromIntegral $ d2int x
                                 v = a * b^e
                              in v + go b (e+1) xs

  integer2Num :: Int -> Integer -> Number
  integer2Num b i | i < fromIntegral b = Number (pred $ int2d b) [int2d (fromIntegral i)] 
                  | otherwise          = Number (pred $ int2d b) (go i)
    where go i = case i `quotRem` (fromIntegral b) of
                   (0,r) -> case integer2Num b r of
                              Number _ rs -> rs
                   (q,r) -> case integer2Num b r of
                              Number _ rs -> (go q) ++ rs

  add :: Number -> Number -> Number
  add (Number u [Zero]) (Number _ ds) = Number u ds
  add (Number u ds) (Number _ [Zero]) = Number u ds
  add a b = add (succ a) (pred b)

  add2 :: Number -> Number -> Number
  add2 (Number u as) (Number _ bs) = Number u (reverse $ go (reverse as) (reverse bs))
    where  go [] ys          = ys
           go xs []          = xs
           go (x:xs) (y:ys)  = case  add (Number u [x]) (Number u [y]) of
                                     Number _ [_,r]  -> r : go xs (go ys [One])
                                     Number _ [r]    -> r : go xs ys
                                     _               -> undefined


  sub :: Number -> Number -> Number
  sub (Number u ds) (Number _ [Zero]) = Number u ds
  sub (Number u [Zero]) (Number _ ds) = error "subtracting beyond zero!"
  sub a b = sub (pred a) (pred b)

  sub2 :: Number -> Number -> Number
  sub2 (Number b as) (Number _ bs)  
    |  as `cmp` bs == LT  = undefined
    |  otherwise          = Number b (clean $ reverse (go (reverse as) 
                                                          (reverse bs)))
    where  go xs []        = xs
           go [] _         = undefined
           go (x:xs) (y:ys)  
             | y > x       =  case sub (Number b [One,x]) 
                                       (Number b [y]) of
                                Number _ [r] -> r : go xs (inc b ys)
             | otherwise   =  case sub (Number b [x])
                                       (Number b [y]) of
                                Number _ [r] -> r : go xs ys
           inc _ []          = [One]
           inc b (x:xs) | x >= b    = Zero : inc b xs
                        | otherwise = succ x : xs

  clean :: [Digit] -> [Digit]
  clean = dropWhile (== Zero)

  cmp :: [Digit] -> [Digit] -> Ordering
  cmp x y = case  lencmp x y of
              GT  -> GT
              LT  -> LT
              EQ  -> go x y
    where  go [] []  = EQ
           go (a:as) (b:bs)  | a > b      = GT
                             | a < b      = LT
                             | otherwise  = go as bs
           go _  _   = undefined

  lencmp :: [a] -> [a] -> Ordering
  lencmp [] []          = EQ
  lencmp [] _           = LT
  lencmp _  []          = GT
  lencmp (_:xs) (_:ys)  = lencmp xs ys
