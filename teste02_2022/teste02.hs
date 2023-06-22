import Data.List

-- 1
-- FUNCIONANDO
analise :: String -> [(Char, Int)]
analise texto = [(c, contar texto c) | c <- texto]

contar :: String -> Char -> Int
contar texto caractere
  | texto == "" = 0
  | caractere == head texto = 1 + contar (tail texto) caractere
  | otherwise = contar (tail texto) caractere

-- 2
-- YOUTUBE
-- FUNCIONANDO
compara::(Ord a, Ord b) => (a, b) -> (a,b) -> Ordering
compara (a1,b1) (a2,b2)
  | b1 < b2 = GT -- significa que b2 é maior que b1
  | b1 == b2 = EQ -- significa que eles são iguais
  | otherwise = LT -- significa que b2 é menor que b1

ordena::String->String
ordena lista = elimina (imprimeCabeca (sortBy compara (analise lista))) ""

elimina :: String -> String -> String
elimina lista finalLista
  | lista == "" = finalLista
  | finalLista == "" = elimina (tail lista) [head lista]
  | head lista == last finalLista = elimina (tail lista) finalLista
  | otherwise = elimina (tail lista) (finalLista ++ [head lista])

imprimeCabeca::[(Char,Int)]->String
imprimeCabeca lista 
  | length lista == 0 = []
  | otherwise = [primeiroElemento (head lista)] ++ imprimeCabeca (tail lista)

primeiroElemento::(a,b) -> a 
primeiroElemento (x, _) = x

segundoElemento::(a,b) -> b 
segundoElemento (_, y) = y




{--
-- ChatGPT

-- Função para contar a ocorrência de cada letra em uma string
countLetters :: String -> [(Char, Int)]
countLetters str = map (\x -> (head x, length x)) (group (sort str))

-- Função para ordenar a lista de acordo com a contagem das letras
sortByLetterCount :: String -> String
sortByLetterCount str = concatMap (\(c, n) -> replicate n c) (sortBy (comparing (negate . snd)) (countLetters str))

-- Exemplo de uso
main :: IO ()
main = do
  let input = "aabbbccc"
  let sortedString = sortByLetterCount input
  putStrLn sortedString
--}
