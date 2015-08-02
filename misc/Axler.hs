module Axler
where

  import Complex
  
  import Debug.Trace (trace)

  ------------------------------------------------------------------------
  -- Exercises 1.1
  ------------------------------------------------------------------------
  -- 1 / (a + bi) = c + di
  -- c + di is the multiplicative inverse of a + bi
  -- thus: (a+bi)(c+di) = unity
  --       (c+di) = (1+0i) / (a+bi)
  -- division of 2 number (a + bi) / (c + di) = 
  --                      (ac + bd / c^2 + d^2) + ((bc -ad)/(c^2 + d^2))i
  -- hence:
  --       (1a + 0b) / (a^2 + b^2) + ((0a - 1b)/(a^2 + b^2))i
  --     = a / (a^2+b^2) - (b/(a^2 + b^2))i 
  --
  -- This exercise is a nice test for cdiv!
  ex1_1 :: Double -> Double -> Bool
  ex1_1 x y = let a  = Complex x y
                  a' = unity / a
               in a' == Complex (x / (x^2+y^2)) (-y/(x^2+y^2))
  
  ------------------------------------------------------------------------
  -- Exercises 1.2
  -------------------------------------------------------------------------
  -- (-1 + sqrt(3)i)^3 = 8, but this is not easy to show with Double
  -- I need a CAS!!!
  --
  -- (1 - sqrt(3)i - sqrt(3)i -3) (-1 + sqrt(3)i)
  -- = (-1 + sqrt(3)i + sqrt(3)i + 3 + sqrt(3)i + 3 + 3 - 3sqrt(3)i)
  -- = 8 + 3sqrt(3)i - 3sqrt(3)i
  -- = 8
  -------------------------------------------------------------------------
  ex1_2 :: (Complex Double,Complex Double)
  ex1_2 = let n = Complex (-1) (sqrt 3)
              d = Complex 2.0 0.0
           in (n^3,d^3)

  ------------------------------------------------------------------------
  -- Exercises 1.3
  -------------------------------------------------------------------------
  -- Prove -(-v) = v
  -- idea: 
  -- -a + a = 0
  -- set a = -v
  -- -(-v) + (-v) = 0
  -- -(-v) - v = 0    | +v
  -- -(-v)     = v
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.4
  -------------------------------------------------------------------------
  -- a in F and v in V
  -- Prove that, if av = 0, then either a = 0 or v = 0
  -- Suppose a is not 0:
  -- av = 0 | * 1/a
  -- v  = 0
  -- Thus: if a /= 0, v = 0
  -- Suppose v is not 0
  -- av = 0 | * 1/v
  -- a  = 0
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.5
  -------------------------------------------------------------------------
  -- Subspaces of R^2?
  -- Test: 
  --   - nonempty?
  --   - additive identity in U?
  --   - For all a, b in U: a+b in U?
  --   - For a in R and u in U: au in U?
  --
  -- a) x1 + 2x2 + 3x3 = 0 
  --    yes:
  --      - (0,0,0) in U
  --      - if (x1,x2,x3) in U and (y1,y2,y3) in U then
  --           (x1+y1,2(x2+y2),3(x3+y3)) = 0 and in U
  --      - (ax1,2ax2,3ax3) = 0 and in U
  --
  -- b) x1 + 2x2 + 3x3 = 4
  --    no:
  --     identity is not in U
  --     (4,0,0) in U and (0,4,0) in U, but not (4,4,0)!
  --     (4,0,0) in U, but 0(4,0,0) not in U!
  --
  -- c) x1x2x3 = 0
  --    no:
  --      - (0,0,0) in U
  --      - any (x1,x2,x3) in U if at least one of x1,x3,x3 = 0
  --      - but: (1,1,0) and (1,0,1) in U
  --             (1,1,0) + (1,0,1) = (2,1,1) is not in U!
  -- d) x1 = 5x3
  --    yes:
  --      - (0,0,0) in U
  --      - (5,1,1) in U and (10,1,2) in U and
  --        (5,1,1) + (10,1,2) = (15,2,3) 
  --        in general: (5b,a,b) + (5d,c,d) = (5(b+d),a+c,b+d)
  --      - a(5v2,v1,v2) = (5av2,av1,av2)
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.6
  -------------------------------------------------------------------------
  -- Subset of R^2 closed under addition and additive identiy, but
  --               not a a subspace
  -- Idea: create a subset, closed under addition, 
  --                        but not under multiplication
  --   or: without identity!
  --  
  -- x3 = x1+x2 (this one is a subspace...)
  --  - ex: (1,1,2) + (2,1,3) = (3,2,5)
  --    general: (x1,x2,x1+x2) + (y1,y2,y1+y2) = (x1+y1,x2+y2,x1+y1+x2+y2)
  -- inverse: (1,1,2): (-1,-1,-2) = (-1,-1,-1-1) in U!
  --  a(1,1,2) = (a,a,2a)
  --  a(1,2,3) = (a,2a,3a)
  --
  -- Solution is easy: let U in Z^2, then
  -- 0.5(x,y) not in U 
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.7
  -------------------------------------------------------------------------
  -- Subset of R^2 closed under scalar multiplication, but not a subspace
  -- Tricky:
  --  (x,0) in U and (0,y) in U
  --  hence: (0,0), (a,0), (0,b) in U, but not (1,1)
  --  a(x,0) = (ax,0) in U
  --  a(0,y) = (0,ay) in U, but
  --  (x,0) + (0,y) = (x,y) is NOT in U if x and y /= 0
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.8
  -------------------------------------------------------------------------
  -- Intersection of subspaces of V is a subspace of V
  --  - 0 in U1 and 0 in U2,
  --  - therefore 0 in U1 \cap U2
  --  - if x and y in U1, then x+y in U1
  --    if x and y also in U2, then x+y in U2 as well
  --    otherwise not in U1 \cap U2
  --  - if (x,y) in U1 then (ax,ay) also in U1 and
  --       (x,y) in U2 then (ax,ay) also in U2
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.9
  -------------------------------------------------------------------------
  -- Union of two subspaces of V is a subspace only if one is contained
  --       in the other
  -- - if a and b in U1, but not in U2 and
  -- -    c and d in U2, but not in U1, then
  --      a,b,a+b, c, d, c+d in U1 \cup U2
  --      then a+c, a+d, a+b+c, a+b+d,a+c+d,b+c+d must all be in U1 \cup U2.
  --      this is the case, if they had been in U1 or U2 from the beginning.
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 1.10
  -------------------------------------------------------------------------
  -- suppose U is a subspace, what is U+U
  -- U+U = U, because
  -- - the additive identity is in U
  -- - any sum x+y in U is in U
  -- - any sum 0+x in U
  -------------------------------------------------------------------------
