contImpar::Int->Int
contImpar n 
  |n < 10 && not (impar n) = 0
  |n < 10 && impar n = 1 + contImpar (div n 10)
  |otherwise = contImpar (div n 10)

impar::Int->Bool
impar n
  |mod n 2 /= 0 = True
  |otherwise = False