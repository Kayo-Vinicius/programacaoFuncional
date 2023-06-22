import Data.Char

-- 1
-- FUNCIONANDO

totalFiltro :: Eq a => (a -> Bool) -> [a] -> Int
totalFiltro _ [] = 0
totalFiltro funcao lista
  | funcao (head lista) = 1 + totalFiltro funcao (tail lista)
  | otherwise = totalFiltro funcao (tail lista)

-- 2
-- FUNCIONANDO
descomprimir :: [(a, Int)] -> [a]
descomprimir tupla = concatena (descomprimirAux tupla)
  where
    descomprimirAux :: [(a, Int)] -> [[a]]
    descomprimirAux tupla = [contar c | c <- tupla]

contar :: (b, Int) -> [b]
contar tupla
  | segundoElemento tupla > 0 = [primeiroElemento tupla] ++ contar (primeiroElemento tupla, (segundoElemento tupla - 1))
  | otherwise = []

concatena :: [[a]] -> [a]
concatena lista = concat lista

primeiroElemento :: (a, b) -> a
primeiroElemento (x, _) = x

segundoElemento :: (a, b) -> b
segundoElemento (_, y) = y