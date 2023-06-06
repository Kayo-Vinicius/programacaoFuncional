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
--    | head lista == item = 1 + contador item (tail lista)
--    | otherwise = contador item (tail lista)

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