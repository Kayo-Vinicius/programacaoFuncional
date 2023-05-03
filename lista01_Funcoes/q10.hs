--NÃO FUNCIONANDO

totiente::Int->Int
totiente n = totiente n 1
  where
    totiente::Int->Int->Int
    totiente cont num
      |num==n && primo num = cont
      |primo num = totiente (cont+1) (num-1)
      |otherwise = totiente cont (num-1)

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

-- CHAT

totient :: Int -> Int
totient n = totient' n 1
  where
    totient' 1 result = result
    totient' k result
      | gcd k n == 1 = totient' (k-1) (result + 1)
      | otherwise = totient' (k-1) result

--FUNCIONANDO
totienteEuler::Int->Int
totienteEuler n = contTotiente n (n-1)

coPrimo::Int->Int->Int->Bool
coPrimo n d i
  |i==1 = True
  |mod n i == 0 && mod d i == 0 = False
  |otherwise = coPrimo n d (i-1)

contTotiente::Int->Int->Int
contTotiente n a
  |a==1 = 1
  |coPrimo n a a = 1 + contTotiente n (a-1)
  |otherwise = contTotiente n (a-1)