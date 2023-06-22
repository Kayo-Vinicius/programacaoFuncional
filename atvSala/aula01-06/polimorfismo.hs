-- analise::String->[(Char,Int)]
-- analise texto = [(c, (contar c texto))|c<-texto]

analise ::Eq t => [t] -> [(t, Int)]
analise texto = [(c, (contar c texto)) | c <- texto]

contar :: Eq a => a -> [a] -> Int
contar c str
  | str == [] = 0
  | c == head str = 1 + contar c (tail str)
  | otherwise = contar c (tail str)

fazPar :: [t] -> [u] -> [(t, u)]
fazPar n m = [(a, b) | a <- n, b <- m]

unZip :: [(u, v)] -> ([u], [v])
unZip lista = ([fst e | e <- lista], [snd e | e <- lista])

concatena :: [[t]] -> [t]
concatena [] = []
concatena (a : x) = a ++ concatena x