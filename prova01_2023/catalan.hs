fatorial::Int->Int
fatorial n
  |n==0 = 1
  |otherwise = n * fatorial (n-1)

--A
nCatalan::Int->Int
nCatalan n 
  |n==1 = 1
  |otherwise = div (fatorial (2*n)) (fatorial (n+1) * fatorial n)


--B
pertenceCatalan::Int->Int->Int->Bool
pertenceCatalan n contador compara -- contador e comprara inicia com 1
  |compara == n = True
  |compara > n = False
  |otherwise = pertenceCatalan n (contador+1) (nCatalan contador)


--C
qtdCatalan::Int->Int->Int
qtdCatalan n compara = auxQtdCatalan n (compara-1)
  where
    auxQtdCatalan::Int->Int->Int
    auxQtdCatalan n compara
      |compara==1 = 2
      |pertenceCatalan compara 1 1 = 1 + auxQtdCatalan n (compara-1)
      |otherwise = auxQtdCatalan n (compara-1)

--D
somaCatalan::Int->Int->Int
somaCatalan n compara -- n recebe o menor valor e compara recebe o maior valor
  |n>compara = 0
  |n==1 = 1 + n + somaCatalan (n+1) compara
  |n==compara && pertenceCatalan compara 1 1 = compara
  |n < compara && pertenceCatalan n 1 1 = n + somaCatalan (n+1) compara
  |otherwise = somaCatalan (n+1) compara