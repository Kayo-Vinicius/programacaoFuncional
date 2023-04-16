x = 1
y = 3
z = 1

def fatorial(x, y, z):
    if( x != y):
        #print('PASSOU AQUI 1')
        print(f'X = {x}')
        print(f'Y = {y}')
        print(f'Z = {z}')
        print()
        z = x * (x + 1)
        return x * fatorial(x + 1, y, z + 1)
    elif (x == y):
        return 1
    elif x == 0:
        return 1
   
print(f'FATORIAL = {fatorial(x, y, z)}')