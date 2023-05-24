l = [1,2,3,4]
r = [2*a | a <- l] --multiplica os elemenstos da lista por 2

s = [a | a <- l, mod a 2 == 0] --imprime apenas os numeros pares

y = [2*a | a <- l, mod a 2 == 0] --multiplica por 2 se a for par

somaParNum::[(Int, Int)] -> [Int]
somaParNum paresNum = [a+b | (a, b) <- paresNum]

divisoresNum::Int->[Int]
divisoresNum n = [a | a <- [1..n-1], mod n a == 0]

nperfeito::Int->Bool
nperfeito n = sum(divisoresNum n) == n 

perfeitos::Int->[Int]
perfeitos n = [x | x <- [1..n], nperfeito x]

concatena::[[Int]]->[Int]
concatena lista = [x | sub <- lista, x <- sub]