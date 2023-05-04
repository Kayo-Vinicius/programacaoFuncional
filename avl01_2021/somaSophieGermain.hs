--FUNCIONANDO
--D

somaSophieGermain::Int->Int->Int
somaSophieGermain n1 n2
  |n1==n2 && sophieGermain n2 = n2
  |n1==n2 = 0
  |sophieGermain n1 = n1 + somaSophieGermain (n1+1) n2
  |otherwise = somaSophieGermain (n1+1) n2

sophieGermain::Int->Bool
sophieGermain n 
  |primo n && primo ((2 * n) + 1) = True
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