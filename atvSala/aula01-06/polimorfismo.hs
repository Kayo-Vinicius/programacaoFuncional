concatena::[[a]]->[b]
concatena [] = []
concatena (x:xs) = x ++ concatena xs  

unZip::[(a,b)]->([a],[b])
--unZip [] = ()
--unZip (a, b) = [(x, y) | x <- a , y <- b]
unZip lista = [fst e | e <-]