multPrimo::Int->Int
multPrimo n 
  |n<10 && primo n = n 
  |n<10 && not (primo n) = 1
  |primo (mod n 10) = mod n 10 * multPrimo (div n 10)
  |otherwise =  multPrimo (div n 10)

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