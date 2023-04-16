x = 12345

def somaQtdNum(x):
    if (x >= 10):
        print('PASSOU AQUI 1')
        print(f'X = {x}')
        return 1 + somaQtdNum(x / 10)
    elif (x < 10):
        print('PASSOU AQUI 2')
        return 1
    elif (x == 0):
        return 1

print(f'SOMA = {somaQtdNum(x)}')

# Pedro
'''
y = 12345
z = 10

def soma(y, z):
    if (y / z >= 10):
        print('PASSOU AQUI 1')
        print(f'Y = {y}')
        print(f'Z = {z}')
        return 1 + soma(y, z * 10 )
    elif (y / z < 10):
        print('PASSOU AQUI 2')
        return 1 + 1
    elif (y == 0):
        return 1

print(soma(y, z))
'''
# Marcelo
'''
def algarismo(n : int):
  aux(n, 10)

def aux(n : int, divisor : int):
        if ((  eturn 0
        else:
            return 1 + aux(n, divisor * 10)
            

teste = print("Informe um número: ")
print(algarismo(teste))
'''
