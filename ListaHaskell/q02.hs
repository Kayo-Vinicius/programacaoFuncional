nPrimo::Int->Int
nPrimo n
  |ehPrimo n == True = n+1

ehPrimo::Int->Bool
ehPrimo n
  |n ´mod´ 2 == 0 = False
  |otherwise = True