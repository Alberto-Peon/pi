prime :: (Integral a) => a -> Bool
prime a
  | a < 2 = False
  | otherwise = null [d | d <- [2 .. floor (sqrt (fromIntegral a))], a `mod` d == 0]

primes :: [Integer]
primes = filter prime [2 ..]

main :: IO ()
main = do
  putStrLn ("primes: " ++ show (take 100 primes))
