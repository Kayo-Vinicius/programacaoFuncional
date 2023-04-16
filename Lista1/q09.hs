gold :: Int->Int
gold n = buscaPar n 1

buscaPar :: Int->Int->Int->Int
buscaPar n p1 p2 
  |n==p1+p2 = 1
    primo p n-primo p
  |otherwise = buscaPar n p+1

aux :: Int->Int->Bool
aux n i
   | i == 1 = True
   | mod n i == 0 = False
   | otherwise = aux n (i-1)

primo :: Int->Bool
primo x
   | x == 1 = False
   | x == 2 = True
   | otherwise = aux x (x-1)