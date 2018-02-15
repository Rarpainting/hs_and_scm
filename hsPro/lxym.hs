fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)
