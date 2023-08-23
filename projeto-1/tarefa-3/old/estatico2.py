lista = [1,2,3,0,0,5,98,7,-4,-5,-10,-8,-8,4,9,12]
M=5
for i in range(M):
    menor = lista[0]
    for j in range(len(lista)):
        if lista[j] < menor:
            aux = menor
            menor = lista[j]
            lista[j] = aux
    print(menor)