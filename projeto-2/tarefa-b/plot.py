import matplotlib
matplotlib.use('tkagg')
import matplotlib.pyplot as plt
from scipy.io import FortranFile
import numpy as np


def readf_xy(filePath: str):
    data = []
    with open(filePath, 'r') as f:
        for line in f:
            data.append(line.rstrip().split())
    return data

data = readf_xy('saida-b-13687303-hist-p1over2.dat')
data = np.array(data)

f1, ax1 = plt.subplots()
ax1.plot(data[:, 0], data[:, 1], label='p=$\frac{1}{2}$')
ax1.grid()
ax1.legend()
f1.show()