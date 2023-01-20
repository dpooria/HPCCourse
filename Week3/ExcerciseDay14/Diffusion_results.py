
import numpy as np
import matplotlib.pyplot as plt


with open("res.txt", "r") as f:
    data = '\\n'.join(f.readlines())

data = data.split('\n')

print(len(data))
print(data[0])


