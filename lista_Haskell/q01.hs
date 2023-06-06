--FUNCIONANDO
potencia::Int->Int->Int
potencia x y
  |x==0 = 1
  |x==1 = 1
  |y==0 = 1
  |otherwise = x * potencia x (y-1)