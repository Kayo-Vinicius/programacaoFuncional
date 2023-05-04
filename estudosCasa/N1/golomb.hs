golomb::Int->Int
golomb n
  |n==1 = 1
  |otherwise = 1 + golomb(n - golomb(golomb(n-1)))