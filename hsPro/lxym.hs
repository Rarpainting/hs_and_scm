-- |/otherwise cond/else
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- : cons
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

{-
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"
-}

for array func = map func array
