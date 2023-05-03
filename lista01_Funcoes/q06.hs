{-Funcionando-}
fatAlt :: Int->Int
fatAlt x
  |x==0 = 1
  |otherwise = fatAlt(x-1)*(-x)

somaFatAlt :: Int->Int
somaFatAlt n
  |n==0 = 1
  |otherwise = somaFatAlt (n-1) + fatAlt n
