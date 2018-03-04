lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number Seven!"
lucky x = "Sorry, you're out of luck"

addvectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addvectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-
first :: (a, b, c) -> a
first (a, _, _) = a
second :: (a, b, c) -> b
first (_, b, _) = b
third :: (a, b, c) -> c
first (_, _, c) = c
-}

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- where 关键字
-- 绑定语法结构
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- let 关键字
-- 绑定表达式
-- let [expression] in
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sidArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sidArea + 2 * topArea

-- case expression
head' :: [a] -> a
head' xs = case xs of
  [] -> error "Not head for empty lists"
  (x:_) -> x
