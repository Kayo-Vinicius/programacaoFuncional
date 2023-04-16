x = 6
y = 1
z = 1

def somaPrimo( x, y, z):
    if(x != y):
        if(x % y == 0):
          print(f'X = {x}')
          print(f'Y = {y}')
          print(f'Z = {z}')
          print()
          return somaPrimo(x, y + 1, z + 1)
        elif(x % y == 1):
            return somaPrimo(x, y + 1, z)
    elif(z > 2):
        return 1

print(f'SOMAPRIMO = {somaPrimo(x, y, z)}')