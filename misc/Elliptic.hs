module Elliptic
where

  import Prelude hiding (mod)
  import System.Random (randomRIO)

  import Prime
  import Modular hiding (add,mul,mDiv)
  import Numerals

  import Debug.Trace (trace)

  -------------------------------------------------------------------------
  -- Point with Identity
  -------------------------------------------------------------------------
  data Point = O | P Integer Integer 

  -------------------------------------------------------------------------
  -- Show points as (x,y)
  -------------------------------------------------------------------------
  instance Show Point where
    show O       = "O"
    show (P x y) = "(" ++ show x ++ "," ++ show y ++ ")"

  -------------------------------------------------------------------------
  -- Equality
  -------------------------------------------------------------------------
  instance Eq Point where
    (P  px py) == (P  qx qy) = px == qx && py == qy
    O == O = True
    _ == _ = False

  -------------------------------------------------------------------------
  -- Point creator taking care of making coordinates modular
  -------------------------------------------------------------------------
  point :: Curve -> (Integer,Integer) -> Point
  point c (x,y) = P (x `mod` p) (y `mod` p)
    where p = curM c 

  -------------------------------------------------------------------------
  -- x-coordinate
  -------------------------------------------------------------------------
  xco :: Point -> Integer
  xco O = error "O has no coordinates"
  xco (P  x _) = x

  -------------------------------------------------------------------------
  -- y-coordinate
  -------------------------------------------------------------------------
  yco :: Point -> Integer
  yco O = error "O has no coordinates"
  yco (P _ y) = y

  -------------------------------------------------------------------------
  -- Curve: coefficients a and b and modulo p (for prime numbers)
  --        y^2 == x^3 + ax + b
  -------------------------------------------------------------------------
  data Curve = Curve {
                 curA :: Integer,
                 curB :: Integer,
                 curM :: Integer}
    deriving (Show,Eq)

  -------------------------------------------------------------------------
  -- Get y for x
  -------------------------------------------------------------------------
  curveY :: Curve -> Integer -> Maybe Integer
  curveY c x = let r = curveY'2 c x
                in if isSqrM r p then Just (findRoot p r)
                                 else Nothing
    where p = curM c

  -------------------------------------------------------------------------
  -- Get y^2 for x
  -------------------------------------------------------------------------
  curveY'2 :: Curve -> Integer -> Integer
  curveY'2 c x = (x^3 + a*x + b) `mod` p
    where a = curA c
          b = curB c
          p = curM c

  -------------------------------------------------------------------------
  -- Find a point on the curve
  -------------------------------------------------------------------------
  findPoint :: Curve -> Point
  findPoint c = let (x,y') = hf [(x, curveY'2 c x) | x <- [1..]]
                 in point c (x,findRoot p y')
    where a = curA c
          b = curB c
          p = curM c
          hf = head . filter (isSqrM p . snd) 

  -------------------------------------------------------------------------
  -- Find the root of a quadratic residue
  -------------------------------------------------------------------------
  findRoot :: Integer -> Integer -> Integer
  findRoot p q = go 0
    where go x | (x^2) `mod` p == q = x
               | otherwise          = go (x+1) 

  -------------------------------------------------------------------------
  -- Ordinary test whether a number is a perfect square
  -------------------------------------------------------------------------
  isSqr :: Integer -> Bool
  isSqr 1 = True
  isSqr x = x `elem` [z^2 | z <- [1..x `div` 2]] 

  -------------------------------------------------------------------------
  -- Test whether a number is a quadratic residue of p
  -------------------------------------------------------------------------
  isSqrM :: Integer -> Integer -> Bool
  isSqrM 0 _ = True
  isSqrM n p = legendre n p == 1

  -------------------------------------------------------------------------
  -- Modular division
  -------------------------------------------------------------------------
  mDiv :: Integer -> Integer -> Integer -> Integer
  mDiv m n d = let d' = inverse d m
                in (n * d') `mod` m

  -------------------------------------------------------------------------
  -- Check if point is on the curve
  -------------------------------------------------------------------------
  oncurve :: Curve -> Point -> Bool
  oncurve _ O     = True
  oncurve c (P x y) = case curveY c x of
                        Nothing -> False
                        Just z  -> y == z   || y == inverse z p ||
                                   y == p-z || y == inverse (p-z) p
    where p = curM c
                    
  -------------------------------------------------------------------------
  -- Addition of points
  -------------------------------------------------------------------------
  add :: Curve -> Point -> Point -> Point
  add c = add2 c (curM c) 

  add2 :: Curve -> Integer -> Point -> Point -> Point
  add2 _ _ q O = q
  add2 _ _ O q = q
  add2 c p q1@(P x1 y1) q2@(P x2 y2) 
    | isInverse c q1 q2 = O
    | otherwise         =
      let l | x1 == x2 {- &&
              y1 == y2 -} = 
              let t1 = (3*x1^2 + a)    `mod` p
                  t2 = inverse ((2*y1) `mod` p) p
               in t1 * t2
            | otherwise = 
              let t1 =          (y2-y1) `mod` p
                  t2 = inverse ((x2-x1) `mod` p) p
               in (t1 * t2) `mod` p
          xr = l^2 - x1 - x2
          yr = l*(x1-xr)-y1 
       in point c (xr, yr)
      where a = curA c

  -------------------------------------------------------------------------
  -- Multiplication of points (naiv)
  -------------------------------------------------------------------------
  mul1 :: Curve -> Integer -> Point -> Point
  mul1 _ _ O = O
  mul1 _ 0 _ = error "multiplication by 0"
  mul1 c n p = go n p
    where go 1 q = q
          go i q = go (i-1) (add c p q)

  -------------------------------------------------------------------------
  -- Multiplication of points (double-and-add)
  -------------------------------------------------------------------------
  mul :: Curve -> Integer -> Point -> Point
  mul _ _ O = O
  mul _ 0 _ = error "multiplication by 0"
  mul c n p = go (tail $ toBinary n) p
    where go [] q     = q
          go (i:is) q = let q' = add c q q
                         in if i == 0 then go is q'
                                      else go is (add c q' p)

  mul2 :: Curve -> Integer -> Integer -> Point -> Point
  mul2 c _ _ O = O
  mul2 c _ 0 _ = error "multiplication by 0!"
  mul2 c h n p = go (tail $ toBinary n) p
    where go [] q     = q
          go (i:is) q = let q' = add c q q
                         in if i == 0 then go is q'
                                      else go is (add2 c h q' p)
  

  -------------------------------------------------------------------------
  -- The inverse of a point (x,y) is (x,-y) mod p
  -------------------------------------------------------------------------
  pinverse :: Curve -> Point -> Point
  pinverse _ O       = O
  pinverse c (P x y) = point c (x,-y)

  -------------------------------------------------------------------------
  -- Check if a point is the inverse of another
  -------------------------------------------------------------------------
  isInverse :: Curve -> Point -> Point -> Bool
  isInverse _ O O = True
  isInverse c p q = q == pinverse c p

  -------------------------------------------------------------------------
  -- Generate the subgroup of point q
  -------------------------------------------------------------------------
  gen :: Curve -> Point -> [Point]
  gen c q = go q
    where go O = [O]
          go r = r:go (add c r q)

  -------------------------------------------------------------------------
  -- Order of the subgroup generated by point q
  -------------------------------------------------------------------------
  gorder :: Curve -> Point -> Integer
  gorder c = fromIntegral . length . gen c 

  -------------------------------------------------------------------------
  -- Primitive elements of c
  -------------------------------------------------------------------------
  primitives :: Curve -> Point -> [Point]
  primitives c g = let h = gen c g 
                       o = maximum (map (gorder c) h)
                    in [q | q <- h, gorder c q == o]

  -------------------------------------------------------------------------
  -- Random Point
  -------------------------------------------------------------------------
  randomPoint :: Curve -> IO Point
  randomPoint c = do
    x <- randomRIO (1,p-1)
    case curveY c x of
      Nothing -> randomPoint c
      Just y  -> return (P x y)
    where p = curM c

  -------------------------------------------------------------------------
  -- Hasse's Theorem
  -------------------------------------------------------------------------
  hasse :: Curve -> (Integer,Integer)
  hasse c = (q + 1 - u, q + 1 + u)
    where q = curM c
          s = sqrt (fromInteger q)
          u = 2*ceiling s

  -------------------------------------------------------------------------
  -- Schoof's Algorithm: The logic
  -------------------------------------------------------------------------
  schoof0 :: Curve -> Point -> IO ()
  schoof0 c p = do
    -- we want to solve the equation
    --
    -- fro^2 - t*fro + q = O where
    --
    -- fro^2 = (x^q^2, y^q^2)
    -- t*fro = (t*x^q,t*y^q)
    -- q     = (q*x,q*y)
    -- 
    let q   = curM c
    let q2  = q^2
    let pf  = frob q p
    let pf2 = P ((xco p)^q2) ((yco p)^q2)
    let qp  = P (xco p * q) (yco p * q)
    let tp  = add2 c 1 pf2 qp
    putStrLn "==============================================="
    putStrLn ("q   is " ++ show q)
    putStrLn ("q^2 is " ++ show q2)
    putStrLn ("qp  is " ++ show qp)
    putStrLn ("Frobenius   of " ++ show p ++ " is " ++ show pf)
    putStrLn ("Frobenius^2 of " ++ show p ++ " is " ++ show pf2)
    putStrLn ("We are searching for t, such that t*pf = " ++ show tp)
    putStrLn "We test with limit 100..."
    let t = go 100 1 q tp pf
    putStrLn ("t = " ++ show t)
    let e = q + 1 - t 
    putStrLn ("e = " ++ show e)
    where go r t q tp pf | r == t    = error "Not found..."
                         | scalar q t pf == tp = t
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
            let tl = frobtr c p l t
                m' = l*m
                t' = -- trace ("m': " ++ show m' ++ ", " ++ show m ++ ", " ++ show t) $
                     (m*tl*(inverse m l) + 
                      l*t *(inverse l m)) `mod` m'
             in go m' t' ls

  frobtr :: Curve -> Point -> Integer -> Integer -> Integer
  frobtr _ O       _ _ = 1
  frobtr c p@(P x y) l t = 
    let h'   = divpoly c l x y 
        h    = if h' == 0 then 1 else abs h'
        pf   = frobm    h q p
        pf2  = frobmn 2 h q p
        qp   = scalar h q p
        tp   = add2 c h pf2 qp -- gives (0,16) instead of (12,16) for (5,1)
     in go 1 tp pf 
    where q = curM c
          go r tp pf | r > q = error ("not found!")
                     | scalar q r pf == tp = r
                     | otherwise = trace ("scalar: " ++ show (scalar q r pf) ++ " == " ++ show tp) $ go (r+1) tp pf

  frobm :: Integer -> Integer -> Point -> Point
  frobm m _ O = O
  frobm 1 q p = frob q p
  frobm m q (P x y) = P ((x^q) `mod` m)  ((y^q) `mod` m)

  frob :: Integer -> Point -> Point
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
  

  --------------------------------------------------------------
  -- Test
  --------------------------------------------------------------
  c1,c2 :: Curve
  c1 = Curve 2  2 17 
  c2 = Curve 2 12 17

  p1,p2 :: Point
  p1 = P 5 1
  p2 = P 4 4
  
  tstadd :: Point
  tstadd = add c1 p1 p1

  tstgen :: Int -> Point -> [Point]
  tstgen i q = go i q
    where go 0 _ = []
          go n p = p:go (n-1) (add c1 q p)

