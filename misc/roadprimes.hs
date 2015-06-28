ldp :: Integer -> Integer
ldp  = ldpf primes 

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes :: [Integer]
-- primes filter prime [3..]            -- this can't be evaluated lazily
-- primes = [x | x <- [2..], prime x]   -- this either

-- primes = 2:[x | x <- [3..], prime x] -- this can be!

primes = 2:filter prime [3..]         -- this also! 

prime :: Integer -> Bool
prime n | n < 1     = error "Not a positive integer!"
        | n == 1    = False
        | otherwise = ldp n == n

main :: IO ()
main = do
  putStrLn ("Primes: " ++ show primes)
