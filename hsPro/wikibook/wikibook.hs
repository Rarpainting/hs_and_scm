f x =
  case x of
    1 -> 2
    2 -> 3
    _ -> (-1)

square x = x^2

point x = (square . f) x

roots a b c =
  let disc = sqrt (b*b - 4*a*c)
      twice_a = 2*a
  in ((-b + disc) / twice_a,
      (-b - disc) / twice_a)
