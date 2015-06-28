-- http://en.wikipedia.org/wiki/Factorization_of_polynomials_over_finite_fields
import Math.Polynomial
import Data.Ratio

gauss :: Int -> IO ()
gauss p = do
  let f = poly BE [1%1, 0%1, 1%1, 0%1, 10%1, 10%1, 8%1, 2%1, 8%1]
  -- squarefree
  -- print $ sqrfree f
  let i     = 1
  print $ go i f [] 
  where go i f s | polyDegree f < 2*i = s
                 | otherwise   =
          let g = gcdPoly f ((powPoly x (pow p i)) `subPoly` x) -- hangs here
           in if g /= one
                then let f' = f `quotPoly` g
                      in go (i+1) f' ((g,i):s)
                else     go (i+1) f s

  {- factorisation of non-squarefree ps
  where go i r c w | w == unity = (r,c)
                   | otherwise  =
          let y      = pgcd w c
              (z,_)  = pdiv w y
              r'     = pmul r (ppow z i)
              (c',_) = pdiv c y
           in go (i+1) r' c' y
  -}

subPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
subPoly a b = a `addPoly` (negatePoly b)

pow :: Int -> Int -> Int
pow b 0 = 1
pow b n = b * pow b (n-1)
