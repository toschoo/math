module Elliptic
where

  import Prelude hiding (mod)
  import System.Random (randomRIO)

  import Prime
  import Modular hiding (add,mul,mDiv)
  import Numerals

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
                in if isSqrM r p then Just (findRoot c r)
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
                 in point c (x,findRoot c y')
    where a = curA c
          b = curB c
          p = curM c
          hf = head . filter (isSqrM p . snd) 

  -------------------------------------------------------------------------
  -- Find the root of a quadratic residue
  -------------------------------------------------------------------------
  findRoot :: Curve -> Integer -> Integer
  findRoot c q = go 0
    where p = curM c
          go x | (x^2) `mod` p == q = x
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
  mDiv m n d = let d' = inverse d' m
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
  add _ q O = q
  add _ O q = q
  add c q1@(P x1 y1) q2@(P x2 y2) | isInverse c q1 q2 = O
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
          p = curM c

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
  -- Schoof's Algorithm
  -------------------------------------------------------------------------
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
     in if x2q /= x_q      then case1 c x y xq yq x2q y2q m
        else if isSqrM q l then case2 c x y xq yq x2q y2q m
                           else 0
    where q = curM c
          a = curA c
          b = curB c

  case1 :: Curve -> Integer -> Integer -> Integer -> Integer ->
                    Integer -> Integer -> Integer -> Integer
  case1 c x y xq yq x2q y2q m = 0

  case2 :: Curve -> Integer -> Integer -> Integer -> Integer ->
                    Integer -> Integer -> Integer -> Integer
  case2 c x y xq yq x2q y2q m = 0

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
  qbar q p = let x = q `mod` p
              in if x > (p `div` 2) then -(x-p)
                                    else x

  divpoly :: Curve -> Integer -> (Integer -> Integer -> Integer)
  divpoly c 0 = \_ _ ->  0
  divpoly c 1 = \_ _ ->  1
  divpoly c 2 = \_ y ->  2*y
  divpoly c 3 = let a = curA c
                    b = curB c 
                 in \x y ->  3*x^4 * (6*a*x^2 + 12*b*x - a^2)
  divpoly c n | even n = let m = n `div` 2
                             a = curA c
                             b = curB c
                             psi0  = divpoly c m
                             psip1 = divpoly c (m+1)
                             psip2 = divpoly c (m+1)
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
                             psip2 = divpoly c (m+1)
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

