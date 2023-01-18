
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit


def f_latency(n, band_width, T_latency):
    return T_latency + n / band_width

data = np.loadtxt('output.dat')
n = data[:, 0]
T = data[:, 1]
bw_avg = np.mean(data[:, 2])
bw_min = np.min(data[:, 3])
bw_max = np.max(data[:, 4])

fit, _ = curve_fit(f_latency, n, T, [1.0,100.0])
band_width = fit[0]
T_latency = fit[1]

plt.scatter(data[:, 0],data[:, 1], label="data",c="orange")
plt.plot(n, f_latency(n, band_width, T_latency), label=f"$T(n)={T_latency:.3e}+(1/{band_width:.3f})n$")
plt.legend()
plt.grid()
plt.xlabel('$n$')
plt.ylabel('$T(n)$')
plt.title('$bw_{avg}=%6.3f$, $bw_{min}=%6.3f$, $bw_{max}=%6.3f$'%(bw_avg, bw_min, bw_max))

plt.savefig('output.jpeg')
