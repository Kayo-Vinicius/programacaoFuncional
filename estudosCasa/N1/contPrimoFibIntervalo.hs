--FUNCIONANDO

contPrimoFibIntervalo::Int->Int->Int
contPrimoFibIntervalo n1 n2 
  |n1 == n2 && primo (fib n2) = 1
  |n1 == n2 = 0
  |primo (fib n1) = 1 + contPrimoFibIntervalo (n1+1) n2
  |otherwise = contPrimoFibIntervalo (n1+1) n2

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

fib::Int->Int
fib n
  |n==0 = 0
  |n==1 = 1
  |otherwise = fib (n-1) + fib (n-2)