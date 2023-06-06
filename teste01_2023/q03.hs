--FUNCIONANDO

somaDigitos::Int->Int
somaDigitos n
  |n<10 = n
  |otherwise = mod n 10 + somaDigitos (div n 10)