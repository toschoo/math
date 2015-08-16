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

  ------------------------------------------------------------------------
  -- Exercises 2.1
  -------------------------------------------------------------------------
  -- if (v_1,...,v_n) spans V, so does (v_1-v_2,v_2-v_3,...,v_n)
  -- That a list of vectors u spans V 
  -- means that for every v in V, v is in u.
  -- To show that v is in u, we have to find scalars a_1..a_n in F,
  -- such that v in (a_1(v_1-v_2) + a_2(v_2-v_3) + ... + a_3v_n).
  -- We multiply that equation out and obtain
  -- a_1v_1-a_1v_2 + a_2v_2-a_2v_3 + ... a_nv_n
  -- and rearrange the terms as
  -- a_1v_1 + (a_2 - a_1)v2 + (a_3 - a_2)v3 + ... + (a_n-a_(n-1))v_n)
  -- This, however, shows that we could have chosen scalars b_1..b_n
  -- such that b_1 = a_1, b_2 = a_2-a_1, ... or
  --                      a_2 = b_2+a_1, ...
  --------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 2.2
  -------------------------------------------------------------------------
  -- if (v_1,...,v_n) is linearly independent in V, so is 
  --    (v_1-v_2,v_2-v_3,...,v_(n-1)-v_n,v_n)
  -- That a list of vectors is linearly independent in V means
  -- that the only choice for scalars a_1,a_2,...,a_n, such that 
  -- (av_1,av_2,...,av_n) = 0 is
  -- a_1 = a_2 = ... = a_n = 0. 
  -- Now consider the list (v_1-v_2,v_2-v_3,...,v_(n-1)-v_n,v_n)
  -- and some list of scalars a_1,a_2,...,a_n, such that
  -- a_1(v_1-v_2) + a_2(v_2-v_3) + ... + a_(n-1)(v_(n-1)-v_n) + a_nv_n = 0.
  -- We again rearrange the equation to
  -- a_1v_1 + (a_2-a_1)v_2 + (a_3-a_2)v_3 + ... + (a_n - a_(n-1))v_n = 0.
  -- It follows:
  -- 1) a_1 = 0
  -- 2) a_2 - a_1 = 0
  -- 3) a_3 - a_2 = 0
  -- ...
  -- Substitute 1 in 2:
  -- a_2 - 0 = 0
  -- a_2     = 0
  --
  -- Substitute 2 in 3 and so on and you get a_1 = a_2 = ... = a_n = 0.
  -- Hence, the list is linearly independent.
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 2.3
  -------------------------------------------------------------------------
  -- If (v_1,...,v_n) is linearly independent in V and for w in V
  --    (v_1+w,v_2+w,...,v_n+w) is linearly dependent, then
  --    w in span(v_1,...,v_n)
  -- That (v_1+w,v_2+w,...v_n+w) is lineary dependent means that
  -- it does not equal (v_1,...,v_n). w, hence, is not 0.
  -- There is a choice for a_1,a_2,...,a_n, 
  -- other than a_1 = a_2 = ... = a_n = 0, such that
  -- a_1(v_1+w) + a_2(v_2+w) + ... + a_n(v_n+w) = 0.
  -- a_1v_1 + a_2v_2 + ... + a_nv_n + (a1+a_2+...+a_n)w = 0
  -- a_1v_1 + a_2v_2 + ... + a_nv_n = -(a1+a_2+...+a_n)w 
  -- (a_1v_1 + a_2v_2 + ... + a_nv_n) / -(a_1+a_2+...+a_n) = w
  -- (v_1/-(a_2+...+a_n)) + (v_2/-(a_1+a_3+...+a_n)) + ... + (v_n/-(a_1+...a_(n-1)) = w
  -- set b_1 = 1/-(a_2+...+ a_n), b_2 = 1/-(a_1+_a_3 + ... + a_n),
  --     ...
  --     b_n = 1/-(a_1+...+a_(n-1)) and we see that w is in span(v_1,...,v_n):
  -- b_1v_1 + b_2v_2 + ... b_nv_n = w.
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 2.4
  -------------------------------------------------------------------------
  -- Is the set {0} ++ P_m(F) a subspace of P(F)?
  -- No. Because it is not closed under addition.
  -- Example: m = 2
  -- p1 = ax^2 + bx + c
  -- p2 = -ax^2 + dx + e
  -- p1+p2 = (b+d)x + c + e, which hase degree < m
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 2.5
  -------------------------------------------------------------------------
  -- F^infinity is infinite dimensional
  -- We have a list of vectors ((1,0,...), (0,1,0,...), (0,0,1,0,...))
  -- that is linearly independent.
  -- The length of any linearly independent list of vectors 
  -- in a finite dimensional vector space is <= any spanning list
  -- of that vector space. However, the list above is infinite.
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 2.6
  -------------------------------------------------------------------------
  -- By fundamental theorem of algebra.
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 3.1
  -------------------------------------------------------------------------
  -- for V, with dim V = 1, show that for every T in L(V,V):
  -- Tv = av, v in V, a in F.
  -- if Tv = 0, then a = 0
  -- for a nonzero vector u, every vector in V is a scalar multiple of u,
  -- i.e. Tu = au;
  -- consider a vector v and a scalar b, such that v = bu. We have 
  -- Tv = Tbu
  -- Tv = bTu
  -- Tv = b(au)
  -- Tv = a(bu) and since bu = v
  -- Tv = av
  -------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- Exercises 3.2
  -------------------------------------------------------------------------
  -- Give an example of a function R2 -> R, such that f(av) = af(v),
  -- but f is not a linear map.
  -- f(x,y) = (x^3+y^3)^(1/3) (Fermat's last theorem)
  -- a = (1,0), b = (0,1)
  -- f(a) + f(b) = f(1,0) + f(0,1) = 
  --   (1+0)^(1/3) + (0+1)^(1/3)   =
  --   1^(1/3) + 1^(1/3) = 1 + 1 = 2
  -- f(a+b) = f((1,0)+(0,1)) = f(1,1) =
  --   (1+1)^(1/3) = 2^(1/3), which, obviously, is not 2
  -------------------------------------------------------------------------

  
