--FUNCIONANDO

somaPrimoCollatz::Int->Int
somaPrimoCollatz n
  |n==1 = 0
  |n==2 = n
  |primo n = n + somaPrimoCollatz ((3*n) + 1)
  |par n = somaPrimoCollatz (div n 2)
  |otherwise = somaPrimoCollatz ((3*n) + 1)

par::Int->Bool
par n
  |mod n 2 == 0 = True
  |otherwise = False

aux::Int->Int->Bool
aux n i
   |i==1 = True
   |mod n i==0 = False
   |otherwise = aux n (i-1)

primo::Int->Bool
primo x
   |x==1 = False
   |x==2 = True
   |otherwise = aux x (x-1)