import Data.Char

-- 1
-- FUNCIONANDO
lista = [1, 2, 3, 6, 7]

pertenceNum :: Int -> [Bool]
pertenceNum n = [True | a <- lista, a == n]

-- 2
-- FUNCIONANDO
comparaMaior :: [Int]-> [Int] -> Int
comparaMaior lista aux
  | lista == [] = head aux
  | aux == [] = comparaMaior (tail lista) [head lista]
  | [head lista] >= aux = comparaMaior (tail lista) [head lista]
  | otherwise = comparaMaior (tail lista) aux

-- 3
-- FUNCIONANDO
nNum :: Int -> [Int] -> Int
nNum n lista = auxNNum n lista 1
  where
    auxNNum:: Int -> [Int] -> Int -> Int
    auxNNum n lista cont 
      | n > length lista = 0
      | cont == n = head lista
      | otherwise = auxNNum n (tail lista) (cont+1) 

-- 6
-- PROFESSOR FEZ
elimina :: String -> String -> String
elimina lista finalLista
  | lista == "" = finalLista
  | finalLista == "" = elimina (tail lista) [head lista]
  | head lista == last finalLista = elimina (tail lista) finalLista
  | otherwise = elimina (tail lista) (finalLista ++ [head lista])

-- 7
-- FUNCIONANDO
duplicaElemento :: [Int] -> String -> [Int]
duplicaElemento lista aux
  | lista == [] = []
  | otherwise = duplicaElemento (tail lista) (aux ++ [head aux])

-- 9
{--
moverDireita :: [Int] -> [Int]
moverDireita [] _ = []
moverDireita list n 
  |moverDireita list 0 = list
  |n == 1 = last list : init list
  |otherwise = moverDireita (moverDireita list) (n-1 )

--}

-- 10
-- FUNCIONANDO
intercede :: [Int] -> [Int] -> [Int]
intercede l1 l2 = [a | a <- l1, b <- l2, a == b]

-- 11
{--
split::Int->[Int]->[(Int, Int)]
split n lista
  |lista head < n = split (tail lista) [head lista]
--}

-- 12
retiraElemento :: Int -> Int -> [Char]

l = ['a', 'b', 'c', 'd', 'e', 'f']

retiraElemento m n = [a | a <- l]

intercala :: [Int] -> [Int] -> [Int]
intercala l1 l2 = [a | a <- l1, a <- l2]

-- 13
duplicatas :: [String] -> [String] -> [String]
duplicatas lista final
  | lista == [] = final
  | final == [] = duplicatas (tail lista) [head final]
  | head lista == last lista = duplicatas (tail lista) (final ++ [last lista])
  | otherwise = duplicatas (tail lista) final

-- 14
-- reg::[(Int, String)]->[(Int, String)]

--18
--metade::[Int]->([Int],[Int])
--metade n = [[a],[b] | a <- [1..(length n 2)], b <- [(length n 2)..n], n == 0]

-- 19
-- FUNCIONANDO
addFim :: Int -> [Int] -> [Int]
addFim n lista
  | lista == [] = [n]
  | otherwise = lista ++ [n]

-- 22
-- FUNCIONANDO
divprop :: Integer -> [Integer]
divprop n = [a | a <- [1 .. n - 1], mod n a == 0]

-- 23
--FUNCIONANDO
somaDiv :: Integer -> Integer -> Integer
somaDiv n i
  | i == 1 = 1
  | mod n i /= 0 = somaDiv n (i - 1)
  | otherwise = i + somaDiv n (i - 1)

perfeito :: Integer -> Bool
perfeito n
  | n == 1 = False
  | n == somaDiv n (n - 1) = True
  | otherwise = False

listaPerfeitos :: Integer -> [Integer]
listaPerfeitos n = [a | a <- [1 .. n], perfeito a]

-- 25
-- PROFESSOR FEZ
cifra :: Int -> String -> String
cifra k texto
  | texto == "" = ""
  | k == 0 = ""
  | otherwise = codifica k (head texto) : cifra k (tail texto)


-- PRECISA FAZER O "import Data.Char"
codifica :: Int -> Char -> Char
codifica k caractere
  | (ord caractere) + k < 65 = chr (90 + (ord caractere) - 64 + k)
  | (ord caractere) + k > 90 = chr (64 + (ord caractere) - 90 + k)
  | ord caractere > 64 && ord caractere < 91 = chr ((ord caractere) + k)
  | otherwise = caractere

-- 28
{--
intersperse::Char->String->String
intersperse simbolo lista
  |lista == "" = ""
 -- |otherwise = intersperse (head [lista] ( ++ simbolo ++ [last lista]))
--}

-- CHATGPT
intercalaS :: Char -> String -> String
intercalaS simbolo string = concatMap (\(x, y) -> [x, simbolo, y]) pares
  where
    pares = zip string (tail string)
