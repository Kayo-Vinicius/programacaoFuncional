palindromo::Int->Bool
palindromo n = (n == inverteNum n)

inverteNum::Int->Int
inverteNum n 
  |n==0 = 0
  |otherwise = ((mod n 10) * (10 ^ expoente (div n 10))) + inverteNum (div n 10)

expoente::Int->Int
expoente n 
  |n==0 = 0
  |otherwise = 1 + expoente (div n 10)