-- http://en.wikipedia.org/wiki/Factorization_of_polynomials_over_finite_fields
-- http://en.wikipedia.org/wiki/Polynomial_division
import Poly

import Data.Ratio

gauss :: Int -> IO ()
gauss p = do
  let f = fromTerms [(8,1%1), (6,1%1), (4,10%1), (3,10%1), (2,8%1), (1,2%1), (0,8%1)]
  -- let f = fromTerms [(2,1%1),(0,-1%1)]
  -- squarefree
  print $ sqrfree f
  let i = 1
  print $ go i f [] 
  where go  i f s | deg f < 2*i = s
                  | otherwise   =
          let g = pgcd f ((ppow xPoly (pow p i)) `psub` xPoly) -- hangs here
           in if g /= unity
                then let (f',_) = f `pdiv` g
                      in go (i+1) f' ((g,i):s)
                else     go (i+1) f s

pow :: Int -> Int -> Int
pow b 0 = 1
pow b n = b * pow b (n-1)
