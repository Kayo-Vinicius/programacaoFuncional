-- 1
-- FUNCIONANDO
filtroTamanho::Int->[String]->[String]
filtroTamanho n lista 
  | lista == [] = []
  | n == 0 = lista
  | n <= calculaTamanho (head lista) = [head lista] ++ filtroTamanho n (tail lista)
  | otherwise = filtroTamanho n (tail lista)

calculaTamanho::String -> Int
calculaTamanho texto 
  | texto == "" = 0
  | otherwise = 1 + calculaTamanho (tail texto)

-- 2
analise :: String -> [String]
analise texto = [contar [c] | c <- texto]

contar :: String -> String
contar caractere
  | caractere /= " " = concatena [caractere]
  | otherwise = concatena [caractere] ++ contar caractere

concatena::[String]->String
concatena lista
  | length lista == 0 = []
  | otherwise = (head lista ++ concatena (tail lista))