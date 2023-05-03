--FUNCIONANDO

passagemAviao::Float->Int->Float
passagemAviao valor idade
  |idade >= 60 = valor * 0.6
  |idade > 2 && idade <= 10 = valor * 0.5 
  |idade <= 2 = valor * 0.1