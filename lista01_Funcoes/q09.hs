--FUNCIONANDO

gold::Int->(Int, Int)
gold n = somaPrimo (n-2) 2
   where
      somaPrimo::Int->Int->(Int, Int)
      somaPrimo p1 p2
         |(primo p1 && primo p2) && p1 + p2 == n = (p1, p2)
         |(primo p1 && primo p2) && p1 + p2 > n = somaPrimo p1 (p2+1)
         |otherwise = somaPrimo (p1-1) (p2+1)
         
aux::Int->Int->Bool
aux n i
   |i==1 = True
   |mod n i == 0 = False
   |otherwise = aux n (i-1)

primo::Int->Bool
primo x
   |x==1 = False
   |x==2 = True
   |otherwise = aux x (x-1)