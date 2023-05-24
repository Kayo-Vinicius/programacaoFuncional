--1
--FUNCIONANDO
lista = [1,2,3,6,7]
pertenceNum::Int->[Bool]
pertenceNum n = [True | a <- lista, a==n]

--2
maiorNum::[Int]
maiorNum = [a | a <- lista]

--3
nNum::Int->[Int]
nNum n = [a | a <- lista, n==0]

--7
--FUNCIONANDO
duplicaElemento::[Int]->[(Int, Int)]
duplicaElemento lista = [(a, a) | a <- lista]

--10
--FUNCIONANDO
intercede::[Int]->[Int]->[Int]
intercede l1 l2 = [a | a <- l1, b <- l2, a==b]

--12
retiraElemento::Int->Int->[Char]
l = ['a','b','c','d','e','f']
retiraElemento m n = [a | a <- l]

intercala::[Int]->[Int]->[Int]
intercala l1 l2 = [a | a <- l1, a <- l2]