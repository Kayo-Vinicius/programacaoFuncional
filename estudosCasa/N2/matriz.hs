--1

matrizIdentidade::Int->[(Int,Int)]
matrizIdentidade n  = [(linha, coluna) | linha <- [1..n], coluna <- [1..n]]