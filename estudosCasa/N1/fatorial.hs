
fatorial :: Int->Int
fatorial n
  |n==0 = 1
  |otherwise = n * fatorial (n-1)

--FATORIAL DUPLO
fatorialDuplo :: Int -> Int
fatorialDuplo n
  | (n == 0) || (n == 1) = 1
  | otherwise = n * fatorialDuplo (n - 2)

--FATORIAL
fatorialQuad :: Int -> Int
fatorialQuad n
  |n==0 = 1
  |otherwise = div (fatorial (2 * n)) (fatorial n)

--PRODUTO DE UM INTERVALO
produtoIntervalo :: Int -> Int -> Int
produtoIntervalo m n
  |m==n = n
  |otherwise = m * produtoIntervalo (m + 1) n

--SUPER FATORIAL
superFatorial :: Int -> Int
superFatorial n
  |n==0 = 1
  |otherwise = (fatorial n) * (superFatorial (n - 1))

--CONTAR OS DIGITOS
contarDigitos :: Int -> Int -> Int
contarDigitos n k
  |n == 0 = 0
  |mod n 10 == k = 1 + contarDigitos (div n 10) k
  |otherwise = contarDigitos (div n 10) k