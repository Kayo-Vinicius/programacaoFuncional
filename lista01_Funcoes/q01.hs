{-Funcionando-}
somaQtdNum::Integer->Integer
somaQtdNum n
  |n==0 = 1
  |n<10 = 1
  |otherwise = 1 + somaQtdNum  (div n 10)