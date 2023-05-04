

somaGemeo::Int->Int
somaGemeo x
  |x < 5 = 0
  |primoGemeo (x-2) x = x + (x-2) + somaGemeo (x-1)
  |otherwise = somaGemeo (x-1)

primoGemeo::Int->Int->Bool
primoGemeo x y
  |primo x  && primo y == True && primo y == (primo (x+2) || primo x == primo (y+2)) = True
  |otherwise = False
  
aux::Int->Int->Bool
aux n i
   |i==1 = True
   |mod n i==0 = False
   |otherwise = aux n (i-1)

primo::Int->Bool
primo x
   |x==1 = False
   |x==2 = True
   |otherwise = aux x (x-1)