--FUNCIONANDO

contPrimoIntervalo::Int->Int->Int
contPrimoIntervalo n1 n2
  |n1==n2 && primo n2 = 1
  |n1==n2 = 0
  |primo n1 && n1 /= n2 = 1 + contPrimoIntervalo (n1+1) n2
  |otherwise = contPrimoIntervalo (n1+1) n2

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