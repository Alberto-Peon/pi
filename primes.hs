primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = null [p | p <- takeWhile (\p -> p * p <= n) primes, n `mod` p == 0]

main :: IO ()
main = do
  putStrLn ("primes: " ++ show (take 100 primes))
