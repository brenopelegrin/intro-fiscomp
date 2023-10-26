def bissecao(f, a, b, e, N):
    i = 1
    fa = f(a)
    while(i<=N):
        c = (b+a)/2.0
        fc = f(c)
        print(i, c)
        i+=1
        if(fa * fc > 0.0):
            a = c
            fa = fc
        else:
            b = c
    print(f"Maximo de interacoes: {i}")

def f(x):
    return x**3 - 4*(x**2) - 59*x + 126

raiz = bissecao(f, -8.0, 0, 1e-6, 7)
print(raiz)
