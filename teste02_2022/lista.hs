import Data.List (nub)

-- 1ª questão
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence n (x : xs)
    | n == x = True
    | otherwise = pertence n xs

-- 2ª questão
maior :: [Int] -> Int
maior [] = 0
maior [a] = a
maior (x : y : xs)
    | x > y = maior (x : xs)
    | otherwise = maior (y : xs)


-- 3ª questão

-- [5, 8 , 9] <- 4
-- [8 , 9] <- 3
-- [9] <- 2
-- [] <- 1
indice :: Int -> [Int] -> Int
-- indice _ [] = error "X"
indice n lista
    | length lista < n = error "Índice fora do intervalo" 
    | n == 0 = head lista
    | otherwise = indice (n-1) (tail lista)

-- [3, 4, 5, 6, 7] -> [4, 5, 6, 7]

-- 4ª questão
-- remove 5 "abcdefg" -> "abcdfg"

remove1 :: Int -> String -> String
remove1 _ [] = []
remove1 n lista
    | n == 1 = tail lista
    | otherwise = head lista : remove1 (n-1) (tail lista)

-- "abc" - 2
-- 'a' : remove1 1 "bc"
-- remove 1 "bc" = "c"
-- 'a' : "c" <=> "ac"

-- length
-- take 
-- drop
-- concat 
-- 5ª questão
reverse' :: String -> String
reverse' [] = []
reverse' (x : xs)
    | null xs = [x]
    | otherwise = reverse' xs ++ [x]

palindromo :: String -> Bool
palindromo lista = lista == reverse' lista

-- duplica 
duplicalista :: [Int] -> [Int]
duplicalista [] = []
duplicalista (cabeca : cauda) = cabeca : cabeca : duplicalista cauda

-- duplicazf :: [Int] -> [Int]
duplicazf lista = concat [[x,x] | x <- lista]

rotacaoDireita :: [Int] -> Int -> [Int]
rotacaoDireita [] _ = []
rotacaoDireita lista 0 = lista
rotacaoDireita lista n
    | n == 1 = last lista : init lista
    | otherwise = rotacaoDireita (rotacaoDireita lista 1) (n-1)


-- 
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