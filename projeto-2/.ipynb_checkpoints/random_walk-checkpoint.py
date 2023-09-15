import random
import matplotlib.pyplot as plt
import numpy as np

walkers = 2
steps = 100

vec_x = [0]*walkers
vec_ms = [0]*walkers

for walker in range(walkers):
    vec_x[walker] = 0.0
    vec_ms[walker] = 0.0
    x = 0
    for step in range(steps):
        randint = random.random()
        if randint > 0.5:
            x = x + 1
            vec_x[walker] = vec_x[walker]+x
            vec_ms[walker] = vec_ms[walker] + x**2
        else:
            x = x - 1
            vec_x[walker] = vec_x[walker]+x
            vec_ms[walker] = vec_ms[walker] + x**2

f1, ax1 = plt.subplots()
ax1.scatter(x=list(range(1,len(vec_x)+1)), y=vec_x)

f1.show()
plt.show()
