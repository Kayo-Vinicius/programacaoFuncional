--FUNCIONANDO

collatz::Int->Int
collatz n 
  |n==1 = 0
  |par n = 1 + collatz (div n 2)
  |otherwise = 1 + collatz ((3*n) + 1)

par::Int->Bool
par n
  |mod n 2 == 0 = True
  |otherwise = False