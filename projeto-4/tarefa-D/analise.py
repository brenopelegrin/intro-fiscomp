#!/usr/bin/env python
# coding: utf-8

# In[42]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# In[43]:


filenames_f0_1 = [
    {'filename': f'd_dados_f0_1_theta0_{j}.csv',
     'label': f'$\\theta_0 = \\theta_{j}$'
    }
        for j in range(1,5)
    ]
filenames_f0_2 = [
    {'filename': f'd_dados_f0_2_theta0_{j}.csv',
     'label': f'$\\theta_0 = \\theta_{j}$'
    }
        for j in range(1,5)
    ]


# In[56]:


f1, ax1 = plt.subplots()
for file in filenames_f0_1:
    df = pd.read_csv(file['filename'], sep=',', header=0)
    ax1.scatter(df['theta'], df['w'], label=file['label'], s=0.5)
ax1.set_xlabel('$\\theta$ (rad)')
ax1.set_ylabel('$\\omega(\\theta)$ (rad/s)')
ax1.grid()
ax1.legend(loc='upper center', bbox_to_anchor=(0.5, -0.12),
          fancybox=True, shadow=True, ncol=5)
f1.savefig('./tarefa-d-13687303-graf1.pdf', dpi=300, bbox_inches='tight')
plt.close()


# ## Gráfico do espaço de fase $\omega(\theta)$ para $F_0 = 1.2$

# In[57]:


f2, ax2 = plt.subplots()
for file in filenames_f0_2:
    df = pd.read_csv(file['filename'], sep=',', header=0)
    ax2.scatter(df['theta'], df['w'], label=file['label'], s=0.5)
ax2.set_xlabel('$\\theta$ (rad)')
ax2.set_ylabel('$\\omega(\\theta)$ (rad/s)')
ax2.grid()
ax2.legend(loc='upper center', bbox_to_anchor=(0.5, -0.12),
          fancybox=True, shadow=True, ncol=5)
f2.savefig('./tarefa-d-13687303-graf2.pdf', dpi=300, bbox_inches='tight')
plt.close()

