--FUNCIONANDO

abonoSalario::Int->Float
abonoSalario pontos
  |pontos == 0 = 0
  |pontos > 1 && pontos <= 10 = 100
  |pontos > 10 && pontos <= 20 = 200
  |pontos > 20 && pontos <= 30 = 300
  |pontos > 30 && pontos <= 40 = 400
  |otherwise = 500
  