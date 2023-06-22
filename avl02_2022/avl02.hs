import Data.List

-- 1
-- FUNCIONANDO
comprime :: String -> String -> Int -> String
comprime lista final contador
  | lista == "" && contador == 1 = final
  | lista == "" = final ++ show contador
  | final == "" = comprime (tail lista) [head lista] (contador + 1)
  | head lista == last final = comprime (tail lista) final (contador + 1)
  | head lista /= last final && contador == 1 = comprime (tail lista) (final ++ [head lista]) 1
  | otherwise = comprime (tail lista) (final ++ show contador ++ [head lista]) 1

-- 2
-- YOUTUBE
ordena :: String -> [(Char, Int)]
ordena lista = verificaIgual (analise lista)

analise :: String -> [(Char, Int)]
analise texto = [(c, contar texto c) | c <- texto]

contar :: String -> Char -> Int
contar texto caractere
  | texto == "" = 0
  | caractere == head texto = 1 + contar (tail texto) caractere
  | otherwise = contar (tail texto) caractere

verificaIgual::[(Char,Int)]->[(Char,Int)]
verificaIgual lista 
    |length lista == 0 = []
    |[primeiroElemento (head lista)] == [primeiroElemento (head (tail lista))] = [head lista] ++ verificaIgual (tail lista)

primeiroElemento::(a,b) -> a 
primeiroElemento (x, _) = x

segundoElemento::(a,b) -> b 
segundoElemento (_, y) = y

-- 2
-- MONITOR FEZ
compressao :: Eq a => [a] -> [a]
compressao [] = []
compressao [x] = [x]
compressao (x : xs)
    | x /= head xs = x : compressao xs 
    | otherwise = compressao xs

compressaoOrdenada :: (Eq a, Ord a) => [a] -> [(a, Int)]
compressaoOrdenada [] = []
compressaoOrdenada lista = juntaSemelhantes (auxCompressaoOrdenada lista 1)

auxCompressaoOrdenada :: (Eq a, Ord a) => [a] -> Int -> [(a, Int)]
auxCompressaoOrdenada [] _ = []
auxCompressaoOrdenada [x] n = [(x , n)] 
auxCompressaoOrdenada (x : xs) n
    | x == head xs = auxCompressaoOrdenada xs (n+1)
    | otherwise = (x, n) : auxCompressaoOrdenada xs 1

juntaSemelhantes :: (Eq a, Ord a) =>  [(a, Int)] -> [(a, Int)]
juntaSemelhantes [] = []
juntaSemelhantes lista = concat ([[y | y <- lista, x == fst y] | x <- unicos [ x | (x , n) <- lista]])

unicos :: (Eq a, Ord a) =>  [a] -> [a]
unicos lista = unicosAux lista []

unicosAux :: (Eq a, Ord a) =>  [a] -> [a] -> [a]
unicosAux [] lista = lista
unicosAux (x : xs) lista
    | x `elem` lista = unicosAux xs lista
    | otherwise = unicosAux xs (lista ++ [x])

-- 2 
-- MARCELO
contador :: Eq a => a -> [a] -> Int
contador item lista
    | length lista == 0 = 0
    | item /= head lista = 0
    | otherwise = 1 + contador item (tail lista)

organizador :: Ord a => [a] -> [(a, Int)]
organizador lista = quickSort (organizadorAUX lista (head lista) 1)

organizadorAUX :: Eq a => [a] -> a -> Int -> [(a, Int)]
organizadorAUX lista anterior aux
    | length lista == 0 = []
    | aux == 1 = [(head lista, contador (head lista) lista)] ++ organizadorAUX (tail lista) (head lista) 0
    | head lista == anterior = organizadorAUX (tail lista) (head lista) 0
    | otherwise = [(head lista, contador (head lista) lista)] ++ organizadorAUX (tail lista) (head lista) 0

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--Questão 1
-- comprimir :: String -> String
-- comprimir "" = ""
-- comprimir (x : xs)
--   | xs == [] = [x]
--   | x /= head xs = x : (comprimir xs)
--   | otherwise = x : (show cont) ++ comprimir (drop cont (x : xs))
--   where
--     cont = contaIguaisInicio x (x : xs)

-- contaIguaisInicio :: Char -> String -> Int
-- contaIguaisInicio _ [] = 0
-- contaIguaisInicio c (x : xs)
--   | c == x = 1 + contaIguaisInicio c xs
--   | otherwise = 0

-- 01
-- POLIMORFICA

comprimir :: (Eq a, Show a) => [a] -> [a]
comprimir [] = []
comprimir (x : xs)
  | xs == [] = [x]
  | x /= head xs = x : comprimir xs
  | otherwise = x : show cont ++ comprimir (drop cont (x : xs)) -- "drop" remove do inicio da lista
  where
    cont = contaIguaisInicio x (x : xs)
    --contStr = show cont

contaIguaisInicio :: (Eq a, Show a) => a -> [a] -> Int
contaIguaisInicio _ [] = 0
contaIguaisInicio c (x : xs)
  | c == x = 1 + contaIguaisInicio c xs
  | otherwise = 0

-- "drop" remove do inicio da lista
-- "take" remove do final da lista

--Questao 2
-- comprimir :: (Eq t, Show t) => [t] -> String
-- comprimir [] = []
-- comprimir (x : xs)
--   | xs == [] = show x
--   | x /= head xs = (show x) ++ (comprimir xs)
--   | otherwise = (show x) ++ (show cont) ++ comprimir (drop cont (x : xs))
--   where
--     cont = contaIguaisInicio x (x : xs)

-- contaIguaisInicio :: Eq t => t -> [t] -> Int
-- contaIguaisInicio _ [] = 0
-- contaIguaisInicio c (x : xs)
--   | c == x = 1 + contaIguaisInicio c xs
--   | otherwise = 0

-- tuplasRepeticao :: Eq t => [t] -> [(t, Int)]
-- tuplasRepeticao [] = []
-- tuplasRepeticao (x : xs)
--   | xs == [] = [(x, 1)]
--   | x /= head xs = (x, 1) : (tuplasRepeticao xs)
--   | otherwise = (x, cont) : (tuplasRepeticao (drop cont (x : xs)))
--   where
--     cont = contaIguaisInicio x (x : xs)

-- filtroTuplas :: Eq t => t -> [(t, Int)] -> [(t, Int)]
-- filtroTuplas elem lista = [(e, n) | (e, n) <- lista, e == elem]

-- ordemAparicao :: Eq t => [t] -> [t]
-- ordemAparicao [] = []
-- ordemAparicao (x : xs) = [x] ++ ordemAparicao [e|e<-xs,e/=x]    

-- resultado::Eq t=>[t]->[(t,Int)]
-- resultado lista = concat [filtroTuplas ele tuplas|ele<-(ordemAparicao lista)]
--   where
--     tuplas =  tuplasRepeticao lista

-- tuplasRepeticao "a" -> [('a',1)]

-- tuplasRepeticao "abc" -> [('a',1),('b',1),('c',1)]

-- tuplasRepeticao "aaaabc" -> [('a',4),('b',1),('c',1)]