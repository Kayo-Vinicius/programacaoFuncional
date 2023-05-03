--FUNCIONANDO

somaPar::Int->Int
somaPar n
  |n < 10 && par n = n
  |n < 10 && not (par n) = 0
  |par (mod n 10) = mod n 10 + somaPar (div n 10)
  |otherwise = somaPar (div n 10)

par::Int->Bool
par n
  |mod n 2 == 0 = True
  |otherwise = False

impar::Int->Bool
impar n
  |mod n 2 /= 0 = True
  |otherwise = False