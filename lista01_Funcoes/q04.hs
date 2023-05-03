--FUNCIONANDO

fatorial :: Int->Int
fatorial n
  |n==0 = 1
  |otherwise = n * fatorial (n-1)

somaFatorial :: Int->Int
somaFatorial n
  |n==0 = 1
  |otherwise = somaFatorial (n-1) + fatorial n