{-FUNCIONANDO-}

nMax::Int->Int->Int->Int
nMax n m p
  |n==m && m==p = 3
  |n==m && m/=p = 2
  |n==p && p/=m = 2
  |p==m && m/=n = 2
  |otherwise = 1