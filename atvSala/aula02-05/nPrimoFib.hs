fibonaci::Int->Int->Int->Int
fibonaci n p s
  |n==1 && primo s = s
  |primo s = fibonaci (n-1) s (p+s)
  |otherwise = fibonaci n s (p+s)

nPrimoFib::Int->Int
nPrimoFib n = fibonaci n 1 1

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