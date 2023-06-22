
-- 1
-- FUNCIONANDO
filtroTamanho :: Int -> [String] -> [String]
filtroTamanho n lista
  | lista == [] = []
  | n == 0 = lista
  | n <= calculaTamanho (head lista) = [head lista] ++ filtroTamanho n (tail lista)
  | otherwise = filtroTamanho n (tail lista)

calculaTamanho :: String -> Int
calculaTamanho texto
  | texto == "" = 0
  | otherwise = 1 + calculaTamanho (tail texto)

-- PROFESSOR FEZ
-- 1
filtro :: Int -> [String] -> [String]
filtro n lista = [palavra | palavra <- lista, length palavra >= n]

-- 2
analise :: String -> [String]
analise texto = [contar [c] | c <- texto]

contar :: String -> String
contar caractere
  | length caractere == 0 = []
  | caractere == " " = []
  | otherwise = concatena [caractere] ++ contar (tail caractere)

concatena :: [String] -> String
concatena lista
  | length lista == 0 = []
  | otherwise = (head lista ++ concatena (tail lista))


-- PROFESSOR FEZ
-- NÃO ESTA FUNCIONANDO
-- 2

analisa :: String -> [(String, Int)]
analisa texto = [(c, contador c (quebrar texto)) | c <- quebrar texto]

quebrar :: String -> [String]
quebrar texto
  | texto == "" = []
  | head texto /= ' ' = [head [texto]] ++ quebrar (tail texto)
  | otherwise = quebrar (tail texto)

contador :: String -> [String] -> Int
contador _ [] = 0
contador c str
  | str == [] = 0
  | c == (head str) = 1 + contador c (tail str)
  | otherwise = contador c (tail str)


-- ChatGPT
-- 2
{--
analisa :: String -> [(String, Int)]
analisa texto = contarPalavras (quebrar texto [])

quebrar :: String -> String -> [String]
quebrar [] palavraAtual = [palavraAtual]
quebrar (c : cs) palavraAtual
  | c == ' ' && palavraAtual /= [] = palavraAtual : quebrar cs []
  | c /= ' ' = quebrar cs (palavraAtual ++ [c])
  | otherwise = quebrar cs []

contarPalavras :: [String] -> [(String, Int)]
contarPalavras [] = []
contarPalavras (p : ps) = (p, contador p (p : ps)) : contarPalavras (removerOcorrencias p ps)

contador :: String -> [String] -> Int
contador _ [] = 0
contador c (p : ps)
  | c == p = 1 + contador c ps
  | otherwise = contador c ps

removerOcorrencias :: String -> [String] -> [String]
removerOcorrencias _ [] = []
removerOcorrencias p (x : xs)
  | p == x = removerOcorrencias p xs
  | otherwise = x : removerOcorrencias p xs
--}