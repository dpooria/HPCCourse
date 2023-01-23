
import numpy as np
import matplotlib.pyplot as plt



data_seq = np.loadtxt('Diffusion_sequential_temperature.txt', dtype=np.float32)
x_seq = data_seq[:, 0]
y_seq = data_seq[:, 1]
temp_seq = data_seq[:, -1]

## This is so slow!!
x_unique = np.unique(x_seq)
y_unique = np.unique(y_seq)
temp_seq_aranged = np.zeros(
    (x_unique.shape[0], y_unique.shape[0]), dtype=np.float32)
temp_mpi_aranged = np.zeros(
    (x_unique.shape[0], y_unique.shape[0]), dtype=np.float32)

for i in range(temp_seq.shape[0]):
    xloc = np.where(x_unique == x_seq[i])[0]
    yloc = np.where(y_unique == y_seq[i])[0]
    temp_seq_aranged[xloc, yloc] = temp_seq[i]

for num_cores in [2, 4, 8]:
    data = np.loadtxt(f"mpi_n{num_cores}.txt")
    x_mpi = data[:, 0]
    y_mpi = data[:, 1]
    temp_mpi = data[:, -1]
    temp_mpi_aranged = np.zeros(
        (x_unique.shape[0], y_unique.shape[0]), dtype=np.float32)
    for i in range(temp_mpi.shape[0]):
        xloc = np.where(x_unique == x_mpi[i])[0]
        yloc = np.where(y_unique == y_mpi[i])[0]
        temp_mpi_aranged[xloc, yloc] = temp_mpi[i]

    print(f"{num_cores} : rms = {np.sqrt(np.sum((temp_mpi_aranged - temp_seq_aranged)**2)/temp_mpi_aranged.size)}")

"""
0.066
"""