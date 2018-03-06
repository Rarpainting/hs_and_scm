-- curried functions
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
{-
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
-}
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter' (>=x) xs)
      biggerSorted = quicksort (filter' (<x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

chain :: (Integral a) => a -> [a]
--chain 0 = [0]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n * 3 + 1)

numLongChains :: Int
-- numLongChains = length (filter' isLong (map' chain [1..100])
--  where isLong = length xs > 15

-- lambda
numLongChains = length (filter' (\xs -> length xs > 15) (map' chain [1..100]))

-- addThree :: (Num a) => a -> a -> a -> a
-- addThree x y z = x + y + z
-- addThree = \x -> \y -> \z -> x + y + z

-- 回传一个绑定了数据的函数
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- fold
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\ace x -> ace + x) 0 xs
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- 累乘
product' :: (Num a) => [a] -> a
product' = foldl1 (*)

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $
-- 将数据作为函数使用
-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- function composition
-- f . g = \x -> f (g x)

-- 最后的 $ 是用来作什么的?
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
