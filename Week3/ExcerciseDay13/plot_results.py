
import numpy as np
import matplotlib.pyplot as plt

data_mpi = np.loadtxt('pi_mpi_fixedIterations_res.dat')
data_seq = np.loadtxt('pi_sequential_res.dat')


scale = data_seq[0]
pi = data_mpi[:, 0]
c_size = data_mpi[:, 1]
time = data_mpi[:, 2]
N = data_mpi[:, 3]

plt.scatter(c_size, scale/time)
plt.plot(c_size, c_size, ":", label="linear scale")
plt.legend()
plt.grid()
plt.axis()
plt.xlabel("Number of nodes")
plt.ylabel("SpeedUp x")
plt.savefig("speedup.jpeg")

