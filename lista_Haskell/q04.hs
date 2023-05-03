-- (A) FUNCIONANDO
restoDiv::Int->Int->Int
restoDiv x y = rem x y

-- (B) FUNCIONANDO
intDiv::Int->Int->Int
intDiv x y = div x y

-- (C) FUNCIONANDO
mdc::Int->Int->Int
mdc x y
  |x==y = x
  |x>y = mdc (x-y) y
  |otherwise = mdc y x

-- (D) FUNCIONANDO
mmc::Int->Int->Int
mmc x y = div (x*y) (mdc x y)