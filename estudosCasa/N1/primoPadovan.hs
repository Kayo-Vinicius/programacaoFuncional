--FUNCIONANDO

primoPadovan::Int->Int
primoPadovan n 
  |n==1 = 0
  |primo (padovan n) = 1 + primoPadovan (n-1)
  |otherwise = primoPadovan (n-1)

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

padovan::Int->Int
padovan n
  |n==0 = 1
  |n==1 = 1
  |n==2 = 1
  |otherwise = padovan (n-2) + padovan(n-3)