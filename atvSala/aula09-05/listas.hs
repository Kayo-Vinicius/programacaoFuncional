somaLista::[Int]->Int
somaLista [] = 0
somaLista (cabeca:cauda) = cabeca + somaLista cauda

replicaChar::Char->Int->[Char]
replicaChar c n 
  |n==0 = []
  |otherwise = c:replicaChar c(n-1)


inverte::[Char]->[Char]
inverte [] = []
inverte (cabeca:cauda) = inverte cauda ++ [cabeca]

ordena::[Int]->[Int]
ordena [] = []
ordena (cabeca:cauda) = insere cabeca (ordena cauda)

insere::Int->[Int]->[Int]
insere x [] = x:[]
insere x (cabeca:cauda)
  |x <= cabeca = x:cabeca:cauda
  |otherwise = cabeca:(insere x cauda)