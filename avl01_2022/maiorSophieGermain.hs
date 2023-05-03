--FUNCIONANDO
--B

maiorSophieGermain::Int->Int
maiorSophieGermain n
  |sophieGermain (n-1) = n-1
  |otherwise = maiorSophieGermain (n-1)

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