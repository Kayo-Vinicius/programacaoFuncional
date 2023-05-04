--FUNCIONANDO

qtdVendas::Int->Int
qtdVendas n
  |n==0 = 5
  |n==1 = 6
  |n==2 = 4
  |n==3 = 8
  |n==4 = 2
  |n==5 = 17
  |otherwise = 0

somaVendas::Int->Int
somaVendas n
  |n==0 = 5
  |n>0 = qtdVendas n + somaVendas (n-1)
  |otherwise = 0