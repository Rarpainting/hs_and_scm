module StringManip where

import Data.Char

uppercase = map toUpper
lowercase = map toLower

capitalize x =
  let capWord [] = []
      capWord (x:xs) = toUpper x:xs
  in unwords (map capWord (words x))
