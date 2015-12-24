module Schoof
where

  import Prelude hiding (mod)
  import System.Random (randomRIO)

  import Prime
  import Modular hiding (add,mul,mDiv)
  import Numerals
  import Elliptic

  import Debug.Trace (trace)

  -------------------------------------------------------------------------
  -- Hasse's Theorem
  -------------------------------------------------------------------------
  hasse :: Curve -> (Integer,Integer)
  hasse c = (q + 1 - u, q + 1 + u)
    where q = curM c
          s = sqrt (fromInteger q)
          u = 2*ceiling s

  -------------------------------------------------------------------------
  -- Find cardinality without Schoof
  -------------------------------------------------------------------------
  preschoof :: Curve -> Point -> Integer
  preschoof c p = go 1 
    where go t | mul c t p == O = t
               | otherwise      = go (t+1)
  
  -------------------------------------------------------------------------
  -- Schoof's Algorithm: The logic
  -------------------------------------------------------------------------
  schoof0 :: Curve -> Point -> IO Integer
  schoof0 c p = do
    -- we want to solve the equation
    --
    -- fro^2 - t*fro + q = O where
    --
    -- fro^2 = (x^q^2, y^q^2)
    -- t*fro = mul c t (x^q,y^q)
    -- q     = mul c q (x,y)
    -- 
    let q   = curM c
    let pf  = frob       q p
    let pf2 = frobmn 2 1 q p
    let qp  = mul2 c 1 q p   -- the problem: we need a modulus: which one?
    let tp  = add2 c 1 pf2 qp 
    putStrLn "==============================================="
    putStrLn ("q   is " ++ show q)
    putStrLn ("qp  is " ++ show qp)
    putStrLn ("Frobenius   of " ++ show p ++ " is " ++ show pf)
    putStrLn ("Frobenius^2 of " ++ show p ++ " is " ++ show pf2)
    putStrLn ("We are searching for t, such that t*pf = " ++ show tp)
    putStrLn "We test with limit 100..."
    let t = go 100 1 q tp pf
    putStrLn ("t = " ++ show t)
    let e = q + 1 - t 
    putStrLn ("e = " ++ show e)
    return t
    where go r t q tp pf | r == t = error "Not found..."
                         | mul2 c q t pf == tp = t -- this is just the same as calculating the whole group
                         | otherwise = go r (t+1) q tp pf

  schoofx :: Curve -> Point -> Integer
  schoofx _ O =  1
  schoofx c p =  let (as,ns) = tryschoofp c p 1 $ drop 1 allprimes
                  in chinese as ns

  tryschoofp :: Curve -> Point -> Integer -> [Integer] -> ([Integer],[Integer])
  tryschoofp c p = go
    where go r (l:ls) | l == q = go r ls
                      | r >  q = ([],[])
                      | otherwise = let t = schoofp c p l
                                     in if 2 == 1 then go r ls
                                        else let (as,ns) = go (r*l) ls
                                              in (t:as,l:ns)
          q = curM c

  -- p must be in the l-torsion subgroup, i.e. lP = O
  schoofp :: Curve -> Point -> Integer -> Integer
  schoofp c p l = 
    let q   = curM c
        q'  = q `mod` l
        fp  = frob       q p
        fp2 = frobmn 2 1 q p
        qp  = mul2 c q q' p  
        tp  = add2 c q fp2 qp 
     in go l 1 q tp fp
    where go r t q tp pf | r == t = 1 -- error "Not found..."
                         | mul2 c q t pf == tp = t
                         | otherwise = go r (t+1) q tp pf
   
  scalar :: Integer -> Integer -> Point -> Point
  scalar _ _ O       = O
  scalar q n (P x y) = P ((n*x) `mod` q) ((n*y) `mod` q) 

  -------------------------------------------------------------------------
  -- Schoof's Algorithm
  -------------------------------------------------------------------------
  schoof2 :: Curve -> Point -> Integer
  schoof2 c p = go 1 0 $ filter (/= q) (drop 1 allprimes)
    where q = curM c
          d = disc c q
          go m t (l:ls) | m > d = t
                        | otherwise =
            let tl = frobtr c p l 
                m' = l*m
                t' | tl == 0 = t -- trace ("m': " ++ show m' ++ ", " ++ show m ++ ", " ++ show t) $
                   | otherwise = 
                     (m*tl*(inverse m l) + 
                      l*t *(inverse l m)) `mod` m'
             in go m' t' ls

  frobtr :: Curve -> Point -> Integer -> Integer
  frobtr _ O         _ = 1
  frobtr c p@(P x y) l = 
    let h'   = divpoly c l x y 
        h    = if h' == 0 then 1 else abs h'
        pf   = frobm    h q p
        pf2  = frobmn 2 h q p
        qp   = mul2 c h q p
        tp   = add2 c h pf2 qp -- gives (0,16) instead of (12,16) for (5,1)
     in go 1 h tp pf 
    where q = curM c
          go r h tp pf | r > q = 0 -- error ("not found!")
                       | mul2 c h r pf == tp = r -- case one
                       | otherwise = {- trace ("scalar: " ++ show (scalar q r pf) ++ " == " ++ show tp) $ -} go (r+1) h tp pf

  frobtr2 :: Curve -> Point -> Integer -> Integer
  frobtr2 _ O         _ = 1
  frobtr2 c p@(P x y) l = 
    let pf   = frobm    l q p
        pf2  = frobmn 2 l q p
        qp   = mul2 c l q' p
        tp   = add2 c l pf2 qp -- gives (0,16) instead of (12,16) for (5,1)
     in go 1 tp pf 
    where q = curM c
          q' = q `mod` l
          go r tp pf | r > l = 0 -- error ("not found!")
                     | mul2 c l r pf == tp = r
                     | otherwise = go (r+1) tp pf

  {-
  mul3 :: Curve -> Integer -> Integer -> Point -> Point
  mul3 _ _ _ O = O
  mul3 c q n (P x y) = 
    let psim1 = divpoly c  
  -}

  gettq :: Curve -> Integer -> Point -> Point
  gettq  _ _ O = O
  gettq  c l p@(P x y) = 
    let h'   = divpoly c l x y 
        h    = if h' == 0 then 1 else abs h'
        pf   = frobm    h q p
        pf2  = frobmn 2 h q p
        qp   = mul2 c h q p
     in add2 c h pf2 qp -- gives (0,16) instead of (12,16) for (5,1)
    where q = curM c

  frobm :: Integer -> Integer -> Point -> Point
  frobm m _ O = O
  frobm 1 q p = frob q p
  frobm m q (P x y) = P ((x^q) `mod` m)  ((y^q) `mod` m)

  frob :: Integer -> Point -> Point
  frob _ O       = O
  frob q (P x y) = P (x^q) (y^q)

  frobmn :: Integer -> Integer -> Integer -> Point -> Point
  frobmn n m q = frobm m (q^n)

  fmul :: (Integer -> Integer) -> Integer -> Integer -> Integer
  fmul _ 0 x = x
  fmul _ 1 x = x
  fmul f n x = fmul f (n-1) (f x)

  schoof1 :: Curve -> Integer -> Point -> Integer
  schoof1 c p (P x y) = 
    let ps  = primel c p
        ts  = map (computeTs c p x y) ps
     in chinese ts ps
    where q = curM c
          a = curA c
          b = curB c

  computeTs :: Curve -> Integer -> Integer -> Integer -> Integer -> Integer
  computeTs c p x y 2 | gcd (x^q - x) (x^3 + a*x + b) == 1 = 1 
                      | otherwise                          = 0
    where q = curM c
          a = curA c
          b = curB c

  computeTs c p x y l = 
    let psl  = (divpoly c p) x y
        q'   = qbar q p 
        m    = gcd (y^2 - x^3 - a*x - b) psl
        ps0  = (divpoly c  q'   ) x y
        psp1 = (divpoly c (q'+1)) x y
        psm1 = (divpoly c (q'-1)) x y
        ps2q = (divpoly c (q'*2)) x y
        xq   = (x^q) `mod` m 
        yq   = (y^q) `mod` m 
        x2q  = (x^(q^2)) `mod` m 
        y2q  = (y^(q^2)) `mod` m 
        x_q  = mDiv m (x - (psm1 * psp1)) (ps0 * ps0)
        y_q  = mDiv m ps2q (2*ps0*ps0*ps0*ps0)
     in trace ("computing " ++ show l) $
        if x2q /= x_q      then case1 c l   x y xq yq x2q y2q x_q y_q m
        else if isSqrM q l then case2 c l q x y xq yq x2q y2q x_q y_q m 
                           else 0
    where q = curM c
          a = curA c
          b = curB c

  case1 :: Curve -> Integer -> 
                    Integer -> Integer -> Integer ->
                    Integer -> Integer -> Integer -> 
                    Integer -> Integer -> Integer -> Integer
  case1 c l x y xq yq x2q y2q x_q y_q m = 
    let x' = ((mDiv m (y2q - y_q) 
                      (x2q - x_q))^2 - x2q - x_q) `mod` m
        y' = case curveY c x' of
               Nothing  -> error "no y"
               Just y'' -> y'' `mod` m
     in 1 -- go 1 x' y' -- this does not make any sense!
    where go t x' y' | t > (l-1) `div` 2 = error "not found!"
                     | x' == xq          = if y' == yq then t else -t
                     | otherwise         = go (t+1) x' y'

  case2 :: Curve -> Integer -> Integer ->
                    Integer -> Integer -> Integer ->
                    Integer -> Integer -> Integer -> 
                    Integer -> Integer -> Integer -> Integer
  case2 c l q x y xq yq x2q y2q x_q y_q m = 
    let w    = findRoot l q
        ps0  = ((divpoly c w) xq yq) `mod` m
        psm1 = ((divpoly c w) xq yq) `mod` m
        psp1 = ((divpoly c w) xq yq) `mod` m
        ps2w = (((divpoly c (2*w)) xq yq)) `mod` m
        (xw,yw) = ((x - (mDiv m (psp1 * psm1) (ps0*ps0))) `mod` m,
                        (mDiv m ps2w (2*ps0*ps0*ps0*ps0)) `mod` m)
    in if xw == x2q 
         then if yw == y2q  
              then   2*w
              else if yw == -y2q then -(2*w) else 0
         else 0 

  primel :: Curve -> Integer -> [Integer]
  primel c p = go 1 (filter (/= p) allprimes)
    where d = disc c $ curM c
          go r (l:ls) | l == p    = go r ls
                      | l*r > d   = [l]
                      | otherwise = let r' = l*r
                                     in if r' > d then [l]
                                                  else l:go r' ls

  disc :: Curve -> Integer -> Integer
  disc _ q = 4*d
    where d = ceiling (sqrt $ fromIntegral q)

  qbar :: Integer -> Integer -> Integer
  qbar q p | q == p    = 1
           | otherwise = let x = q `mod` p
                          in if x > (p `div` 2) then -(x-p)
                                                else x

  divpoly :: Curve -> Integer -> Integer -> Integer -> Integer
  divpoly c 0 = \_ _ ->  0
  divpoly c 1 = \_ _ ->  1
  divpoly c 2 = \_ y ->  2*y
  divpoly c 3 = let a = curA c
                    b = curB c 
                 in \x y -> 3*x^4 + 6*a*x^2 + 12*b*x - a^2
  divpoly c 4 = let a = curA c
                    b = curB c
                 in \x y -> 4*y*(x^6 + 5*a*x^4 + 20*b*x^3 - 
                                 5*a^2*x^2 - 4*a*b*x - 8*b^2 - a^3)
  divpoly c n | even n = let m = n `div` 2
                             a = curA c
                             b = curB c
                             psi0  = divpoly c m
                             psip1 = divpoly c (m+1)
                             psip2 = divpoly c (m+2)
                             psim1 = divpoly c (m-1)
                             psim2 = divpoly c (m-2)
                          in \x y -> ((psi0 x y) `div` (2*y)) * (
                                     (psip2 x y) * (psim1 x y) * (psim1 x y) -
                                     (psim2 x y) * (psip1 x y) * (psip1 x y)) 
              | odd n  = let m = (n-1) `div` 2
                             a = curA c
                             b = curB c
                             psi0  = divpoly c m
                             psip1 = divpoly c (m+1)
                             psip2 = divpoly c (m+2)
                             psim1 = divpoly c (m-1)
                          in \x y -> (psip2 x y) + 
                                     (psi0  x y) * (psi0 x y) * (psi0 x y) -
                                     (psim1 x y) * 
                                     (psip1 x y) * (psip1 x y) * (psip1 x y)
  

