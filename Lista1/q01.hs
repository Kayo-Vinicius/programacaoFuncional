{-Não está funcionando-}
somaQtdNum::Int->Int
somaQtdNum n
  |n==0 = 1
  |n<10 = 1
  |otherwise = 1 + (somaQtdNum  n`div`10)