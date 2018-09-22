primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

prime1234 :: Integer
prime1234 = last (take 1234 primes)
