lista = [1,2,3,0,0,5,98,7,-4,-5,-10,-8,-8,4,9,12]
M=5
menoresIdx = []
for i in range(M):
    menor = lista[0]
    lastMenorIdx = 0
    for j in range(len(lista)):
        if lista[j] < menor and j not in menoresIdx:
            menor = lista[j]
            lastMenorIdx = j
    menoresIdx.append(lastMenorIdx)
    print(menor)