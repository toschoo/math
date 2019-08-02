import Linear
import Poly

main :: IO ()
main = putStrLn (show $ det (sylvester p q))
  where p = P [-5,2,8,-3,-3,0,1,0,1]
        q = P [21,-9,-4,0,5,0,3]
        -- p = P [-5,2,8,-3,-3]
        -- q = P [21,-9,-4]

