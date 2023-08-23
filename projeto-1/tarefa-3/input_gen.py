lista = [1,2,3,0,0,5,98,7,-4,-5,-10,-8,-8,4,9,12]
with open('./input.data', 'w') as f:
    for i in lista:
        f.write(str(float(i))+'\n') 