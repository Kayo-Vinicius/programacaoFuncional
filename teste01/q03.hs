{-Não está funcionando-}
soma::Int->Int
soma n
  |n<10 = n
  |otherwise = (n`mod`10) + (soma n`div`10)