max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  |otherwise = b

maximum' :: (Ord a) => [a] -> a
maximum' xs =
  case xs of
    [] -> error "maximum of empty list"
    [x] -> x
    (x:xxs) -> max x (maximum' xxs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = [] -- 若不满足条件, 直接调到下一匹配
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallSorted ++ [x] ++ biggerSorted
