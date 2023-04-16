{-Funcionando-}
coleta :: Int -> Int
coleta dia
  |dia==1 = 7
  |otherwise  = dia * 7 + coleta(dia-1) 