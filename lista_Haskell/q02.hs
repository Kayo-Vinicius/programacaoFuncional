--FUNCIONANDO

nPrimo::Int->Int
nPrimo n = contPrimo 0 2
   where
      contPrimo::Int->Int->Int
      contPrimo cont num
         |cont==n = num-1
         |primo num = contPrimo (cont+1) (num+1)
         |otherwise = contPrimo cont (num+1)

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

