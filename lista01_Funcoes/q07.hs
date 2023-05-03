{-Funcionando-}
progressaoGeometrica::Int->Int->Int->Int
progressaoGeometrica q a n
  |n==1 = 1
  |otherwise = q * progressaoGeometrica q a (n-1)