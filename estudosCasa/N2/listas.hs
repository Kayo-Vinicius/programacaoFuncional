divisivel20::Int->[Bool]
divisivel20 x = [True | a <- [1..20], mod x a == 0]

--6
--FUNCIONANDO

collatz::Int->Int
collatz n
  |n == 0 = 0
  |mod n 2 == 0 = (div n 2)
  |otherwise = ((3*n) + 1)

--7
--FUNCIONANDO

auxCollatz::Int->[Int]
auxCollatz x
  |x == 1 = []
  |otherwise = [collatz (x)] ++ auxCollatz (collatz x)

--8
collatzCont::Int->Int
collatzCont n 
  |n==1 = 0
  |mod n 2 == 0 = 1 + collatzCont (div n 2)
  |otherwise = 1 + collatzCont ((3*n) + 1)

--maiorSequeciaCollatz::Int->[Int]
--maiorSequeciaCollatz n = [a | a <- [1..100], auxCont ((collatzCont a) (collatzCont a))]

auxCont::Int->Int
auxCont n  = auxContador 1 1
 where
  auxContador::Int->Int->Int
  auxContador n maior
    |n == 30 = maior
    |collatzCont n > collatz maior = auxContador (n+1) n
    |collatzCont n < collatz maior = auxContador (n+1) maior
    |otherwise = auxContador (n+1) n 

maiorNum::Int->Int->Int
maiorNum n1 maiorAtual
  |n1 > maiorAtual = n1
  |otherwise = maiorAtual