-- 1

--A
--FUNCIONANDO
impar::[Int]
impar = [x | x <- [1..100], mod x 2 /= 0]

--B
--FUNCIONANDO
par::[Int]
par = [x | x <- [10..100], mod x 2 == 0]

--C
--FUNCIONANDO
nImpar::Int->[Int]
nImpar n = [x | x <- [1..n], mod x 2 /= 0]

--D
--FUNCIONANDO
multiplos::Int->[Int]
multiplos n = [x | x <- [1..n], mod x 3 == 0, mod x 5 == 0]

--E
--FUNCIONANDO
quadrado::Int->[(Int, Int)]
quadrado n = [(x, x*x) | x <- [1..n]]

--F
--FUNCIONANDO
matriz::[(Int, Int)]
matriz = [(i, j) | i <- [1..3], j <- [1..4]]

--G
--FUNCIONANDO
matrizNM::Int->Int->[(Int, Int)]
matrizNM n m = [(i, j) | i <- [1..n], j <- [1..m]]

--2
--PROFESSOR FEZ
listaFib::Int->[Int]
listaFib n 
  |n == 1 = [1]
  |n == 2 = [1,1]
  |otherwise = listaFib (n-1) ++ [fib n]

fib::Int->Int
fib n 
  |n==1 = 1
  |n==2 = 1
  |otherwise = fib (n-1) + fib (n-2)

--3
hexadecimal::String->String
hexadecimal string
  |length string == 0 = []
  |mod (length string) (4) /= 0 = hexadecimal(['0' |x <- [1..(4 - mod (length string) (4))]] ++ string)
  |otherwise = binToHex (take 4 string) ++ binToHex (drop 4 string)

binToHex::String->String
binToHex "0000" = "0"
binToHex "0001" = "1"
binToHex "0010" = "2"
binToHex "0011" = "3"
binToHex "0100" = "4"
binToHex "0101" = "5"
binToHex "0110" = "6"
binToHex "0111" = "7"
binToHex "1000" = "8"
binToHex "1001" = "9"
binToHex "1010" = "A"
binToHex "1011" = "B"
binToHex "1100" = "C"
binToHex "1101" = "D"
binToHex "1110" = "E"
binToHex "1111" = "F"
binToHex _ = ""

--PROFESSOR FEZ
conv "0000" = "0"
conv "0001" = "1"
conv "0010" = "2"
conv "0011" = "3"
conv "0100" = "4"
conv "0101" = "5"
conv "0110" = "6"
conv "0111" = "7"
conv "1000" = "8"
conv "1001" = "9"
conv "1010" = "A"
conv "1011" = "B"
conv "1100" = "C"
conv "1101" = "D"
conv "1110" = "E"
conv "1111" = "F"

bin2hex::String->String
bin2hex s
   |s=="" = ""
   |mod (length s) 4 /=0 = bin2hex((replicate (4-(mod (length s) 4)) '0') ++ s)
   |otherwise = conv (take 4 s) ++ bin2hex(drop 4 s)

--4
--PROFESSOR FEZ
hanoi::Int->Int->Int->Int->[String]
hanoi q origem aux destino
    |q == 0 = []
    |q == 1 = [show(origem) ++ "->" ++ show(destino)]
    |otherwise = ch1 ++ [show(origem) ++ "->" ++ show(destino)] ++ ch2
        where
            ch1 = hanoi (q-1) origem destino aux
            ch2 = hanoi (q-1) aux origem destino