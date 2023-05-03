--FUNCIONANDO

fibPrimo::Int->Int
fibPrimo n = contFib 1 1
   where
      contFib::Int->Int->Int
      contFib cont num
         |cont==n && primo(fib num) = fib num
         |primo (fib num) = contFib (cont+1) (num+1)
         |otherwise = contFib cont (num+1)

fib::Int->Int
fib n 
   |n==0 = 0
   |n==1 = 1
   |otherwise = fib (n-1) + fib (n-2)

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

-- função para verificar se um número é primo
isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor(sqrt(fromIntegral n))]

-- função para calcular a conjectura de Goldbach
goldbach :: Int -> Maybe (Int, Int)
goldbach n
  | n <= 2 || odd n = Nothing -- retorna Nothing para números ímpares e menores ou iguais a 2
  | otherwise = findPair primes
  where
    primes = filter isPrime [2..n-2] -- lista de todos os números primos menores que n-2
    findPair (x:xs) = case filter (\y -> x + y == n) xs of -- verifica se existe um número primo y tal que x + y == n
                        [] -> findPair xs -- se não encontrar, continua procurando
                        (y:_) -> Just (x,y) -- se encontrar, retorna Just (x,y)
    findPair [] = Nothing -- se não encontrar nenhum par de números primos, retorna Nothing
