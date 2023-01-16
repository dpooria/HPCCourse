
import os
import pathlib
import re
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
from itertools import cycle
from scipy.optimize import curve_fit

fig_size = (12, 10)
font = {'family': 'DejaVu Sans', 'size'   : 16}
matplotlib.rc('font', **font)

def amdahl(n, p):
    speedup = 1.0 / ((1.0 - p) + (p / n))
    return speedup


def fit_amdahl(n, s):
    fit, _ = curve_fit(amdahl, n, s, 0.5)
    return fit[0]


def read_benchmark_data():
    data = []
    for directory in ["Poisson3D_Sequential",
                      "Poisson3D_OMP_V0.1.0", "Poisson3D_OMP_V0.1.1", "Poisson3D_OMP_V0.2.0"]:

        dirpath = pathlib.Path(directory)
        if not dirpath.is_dir():
            print(f"Warning!: directory {directory} not found!")
            continue
        list_of_files = dirpath.iterdir()

        data_pattern = \
            re.compile(
                rf"(jacobi|gaussseidel)(\_\d+threads)?\_N\d+(\_full\_iteration)?(\_unoptimized)?\.dat")
        for file_name in list_of_files:
            matches = data_pattern.finditer(file_name.as_posix())
            for match in matches:
                if match:
                    with open(file_name, "r") as f:
                        first_line = f.readline()
                    dat = first_line.split(',')
                    num_threads = 1
                    match_num_threads = match.group(2)
                    if match_num_threads is not None:
                        num_threads = int(re.findall(
                            r"\d+", match_num_threads)[0])
                    data.append({"file": file_name.as_posix(), "method": match.group(1),
                                "exec_time": float(dat[0]), "iterations": int(dat[1]),
                                 "error": float(dat[2]), "N": int(dat[3]), "num_threads": num_threads,
                                 "is_full_iter": (match.group(3) is not None), "version": directory,
                                 "with_o3flag": (match.group(4) is None)})

    return pd.DataFrame.from_dict(data)


def compare_resource_scalability(data: pd.DataFrame, versions, fname: str, N: int, seq_scale=None):
    # versions = data["version"].unique()
    fig, ax = plt.subplots()
    fig.set_size_inches(12, 10)
    for v in versions:
        dat = data[data["version"] == v]
        if not dat.empty:
            dat = dat.sort_values(by="num_threads")
            e_time = dat["exec_time"].to_numpy()
            if not seq_scale:
                seq_scale = e_time[dat["num_threads"] == 1][0]
            sp = seq_scale / e_time
            p = fit_amdahl(dat["num_threads"].to_numpy(), sp)
            ax.scatter(dat["num_threads"], sp)
            ax.plot(dat["num_threads"], amdahl(dat["num_threads"], p),
                    label=f"{v}:(Amdahl's law, p = {p:.3f})")
    ax.plot(dat["num_threads"], dat["num_threads"], 'k:')
    ax.set_title(f"N={N}")
    ax.set_xlabel("num_threads")
    ax.set_ylabel("speedup x")
    ax.legend()
    ax.grid(True)
    fig.savefig(fname, bbox_inches='tight')
    plt.close(fig)



def contour_temp(versions, level=0, fname="RoomTemperature_Contour.jpeg"):
    fig, axes = plt.subplots(3, 2)
    fig.set_size_inches(6*3, 6*2)
    ax_cycle = cycle(axes.flat)
    for i, v in enumerate(versions):
        ax = next(ax_cycle)
        with open(pathlib.Path(f'{v}').joinpath('fulldata_jacobi.dat'), 'r') as f:
            first_line = f.readline()
            data = np.loadtxt(f, skiprows=1)
        first_line = first_line.split(",")
        exec_time = float(first_line[0])
        iterations = int(first_line[1])
        d = float(first_line[2])
        N = int(first_line[3])
        delta = data[1, 0] - data[0, 0]
        z = data[:, 2]
        level = 0
        sliced_z = data[((z >= (level * delta)) &
                         (z <= ((level + 1) * delta))), :]
        x = sliced_z[:, 0]
        y = sliced_z[:, 1]
        t = ax.tricontourf(x, y, sliced_z[:, 3])
        ax.set_title(
            f"Jacobi*{v}, N={N}, t={exec_time:.3e}sec, iters={iterations}, $\epsilon$={d:.2e}",
            font={"size": 10})
        if (i % 2) == 0:
            ax.set_ylabel('y')

    for i, v in enumerate(["Poisson3D_Sequential", "Poisson3D_OMP_V0.2.0"]):
        ax = next(ax_cycle)
        with open(pathlib.Path(f'{v}').joinpath('fulldata_gaussseidel.dat'), 'r') as f:
            first_line = f.readline()
            data = np.loadtxt(f, skiprows=1)
        first_line = first_line.split(",")
        exec_time = float(first_line[0])
        iterations = int(first_line[1])
        d = float(first_line[2])
        N = int(first_line[3])
        delta = data[1, 0] - data[0, 0]
        z = data[:, 2]
        level = 0
        sliced_z = data[((z >= (level * delta)) &
                         (z <= ((level + 1) * delta))), :]
        x = sliced_z[:, 0]
        y = sliced_z[:, 1]
        t = ax.tricontourf(x, y, sliced_z[:, 3])
        ax.set_xlabel('x')
        if (i % 2) == 0:
            ax.set_ylabel('y')

        ax.set_title(
            f"Gauss-Seidel*{v}, N={N}, t={exec_time:.3e}sec, iters={iterations}, $\epsilon$={d:.2e}",
            font={"size": 10})

    fig.colorbar(t)
    fig.savefig(fname, bbox_inches='tight')
    plt.close(fig)


def compare_convergence(ax, seq_jacobi_notfull, seq_gauss_notfull):
    ax.plot(seq_jacobi_notfull["N"], seq_jacobi_notfull["iterations"])
    ax.scatter(seq_jacobi_notfull["N"],
               seq_jacobi_notfull["iterations"], label="Jacobi")
    ax.plot(seq_gauss_notfull["N"], seq_gauss_notfull["iterations"])
    ax.scatter(
        seq_gauss_notfull["N"], seq_gauss_notfull["iterations"], label="Gauss-Seidel")
    ax.set_xlabel("N")
    ax.set_ylabel("iterations")
    ax.set_title("$\epsilon=10^{-5}$")
    ax.legend()
    ax.grid(True)


def plot_benchmark_over_N(data: pd.DataFrame, fname: str, scale: dict):
    fig, ax = plt.subplots()
    fig.set_size_inches(fig_size)
    data = data.sort_values(by="N")

    # for d in data:
    #     performance.append(scale_N[d["N"]]/d["exec_time"])
    threads_list = data["num_threads"].unique()
    threads_list.sort()
    for threads in threads_list:
        dat = data[data["num_threads"] == threads]
        label = f"N_threads={threads}"
        performance = [
            scale[N]/exec_time for (N, exec_time) in zip(dat["N"], dat["exec_time"])]
        ax.plot(dat["N"], performance)
        ax.scatter(dat["N"], performance, label=label)
    ax.legend()
    ax.grid()
    ax.set_xlabel("N")
    ax.set_ylabel("speed up")
    fig.savefig(fname, bbox_inches='tight')
    plt.close(fig)


def plot_benchmark_over_threads(data: pd.DataFrame, fname: str, scale: dict):
    fig, ax = plt.subplots()
    fig.set_size_inches(fig_size)
    data = data.sort_values(by="num_threads")
    # for d in data:
    #     performance.append(scale_N[d["N"]]/d["exec_time"])
    N_list = data["N"].unique()
    N_list.sort()
    for N in N_list:
        dat = data[data["N"] == N]
        label = f"N={N}"
        performance = scale[N] / dat["exec_time"]
        ax.plot(dat["num_threads"], performance)
        ax.scatter(dat["num_threads"], performance, label=label)
    ax.legend()
    ax.grid()
    ax.set_xlabel("num_threads")
    ax.set_ylabel("speed up")
    fig.savefig(fname, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    version = ["Poisson3D_Sequential",
               "Poisson3D_OMP_V0.1.0",
               "Poisson3D_OMP_V0.1.1",
               "Poisson3D_OMP_V0.2.0"]
    contour_temp(version, level=0,
                 fname="RoomTemperature_Contour_middle.jpeg")
    contour_temp(version, level=-99,
                 fname="RoomTemperature_Contour_floor.jpeg")
    contour_temp(version, level=99, fname="RoomTemperature_Contour_top.jpeg")

    df = read_benchmark_data()
    df.to_csv('benchmark_data.csv')

    df_jacobi = df[df["method"] == "jacobi"]
    df_gauss = df[df["method"] == "gaussseidel"]
    # Compare Jacobi and Gauss-Seidel Convergence
    seq_jacobi_notfull = df_jacobi[(~df_jacobi["is_full_iter"]) & (
        df_jacobi["version"] == version[0])].sort_values(by="N")
    seq_gauss_notfull = df_gauss[(~df_gauss["is_full_iter"]) &
                                 (df_gauss["version"] == version[0])].sort_values(by="N")
    fig, ax = plt.subplots()
    fig.set_size_inches(fig_size)

    compare_convergence(ax, seq_jacobi_notfull, seq_gauss_notfull)
    fig.savefig("Sequential_Jacobi_Gauss_convergence_comparison.jpeg",
                bbox_inches="tight")
    plt.close(fig)
    # Jacobi
    # the scaling of the different versions
    df_jacobi_o3 = df_jacobi[df_jacobi["with_o3flag"]
                             & df_jacobi["is_full_iter"]]
    df_jacobi_withoutO3 = df_jacobi[(
        ~df_jacobi["with_o3flag"]) & df_jacobi["is_full_iter"]]

    sequential_jacobi = df_jacobi_o3[df_jacobi_o3["version"]
                                     == "Poisson3D_Sequential"]

    scale_sequential = {k: v
                        for (k, v) in zip(sequential_jacobi["N"], sequential_jacobi["exec_time"])}

    for v in version[1:]:
        dat = df_jacobi_o3[df_jacobi_o3["version"] == v]
        plot_benchmark_over_N(
            dat, f"Jacobi_{v}_Nbenchmark_sequentialScale.jpeg", scale_sequential)
        plot_benchmark_over_threads(
            dat, f"Jacobi_{v}_threadsbenchmark_sequentialScale.jpeg", scale_sequential)

        dat_single = dat[dat["num_threads"] == 1]
        scale_dat = {k: v for (k, v) in zip(
            dat_single["N"], dat_single["exec_time"])}
        plot_benchmark_over_N(
            dat, f"Jacobi_{v}_Nbenchmark.jpeg", scale_dat)
        plot_benchmark_over_threads(
            dat, f"Jacobi_{v}_threadbenchmark.jpeg", scale_dat)

    #####################
    for v in version[1:]:
        dat = df_jacobi_withoutO3[df_jacobi_withoutO3["version"] == v]

        plot_benchmark_over_N(
            dat, f"Jacobi_{v}_NotCompilerOptimized_Nbenchmark_sequentialScale.jpeg", scale_sequential)

        dat_single = dat[dat["num_threads"] == 1]
        scale_dat = {k: v for (k, v) in zip(
            dat_single["N"], dat_single["exec_time"])}
        plot_benchmark_over_N(
            dat, f"Jacobi_{v}_NotCompilerOptimized_Nbenchmark.jpeg", scale_dat)
        plot_benchmark_over_threads(
            dat,
            f"Jacobi_{v}_NotCompilerOptimized_threadbenchmark_sequentialScale.jpeg",
            scale_sequential)

        plot_benchmark_over_threads(
            dat,
            f"Jacobi_{v}_NotCompilerOptimized_threadbenchmark.jpeg",
            scale_dat)

    # Compare different versions of Jacobi
    # Resource scalability
    df_omp = df_jacobi_o3[df_jacobi_o3["version"] != version[0]]
    df_omp = df_omp.sort_values(by="N")
    N = df_omp["N"].unique()
    N.sort()
    for N_ in N:
        df_n = df_omp[df_omp["N"] == N_]
        compare_resource_scalability(df_n, version[1:],
                                     f"resource_scabality_comparison_N{N_}_sequentialScale.jpeg", N_,
                                     scale_sequential[N_])

        compare_resource_scalability(df_n, version[1:],
                                     f"resource_scabality_comparison_N{N_}.jpeg", N_)
    # Plot unoptimized
    df_omp = df_jacobi_withoutO3[df_jacobi_withoutO3["version"] != version[0]]
    df_omp = df_omp.sort_values(by="N")
    N = df_omp["N"].unique()
    N.sort()
    for N_ in N:
        df_n = df_omp[df_omp["N"] == N_]
        compare_resource_scalability(df_n, version[1:],
                                     f"unoptimized_resource_scabality_comparison_N{N_}_sequentialScale.jpeg", N_,
                                     scale_sequential[N_])

        compare_resource_scalability(df_n, version[1:],
                                     f"unoptimized_resource_scabality_comparison_N{N_}.jpeg", N_)

    # Gauss-Seidel
    v = version[-1]
    df_gauss_o3 = df_gauss[df_gauss["with_o3flag"]
                           & df_gauss["is_full_iter"]]
    df_gauss_withoutO3 = df_gauss[(
        ~df_gauss["with_o3flag"]) & df_gauss["is_full_iter"]]

    sequential_gauss = df_gauss_o3[df_gauss_o3["version"]
                                   == version[0]]

    scale_sequential = {k: v_
                        for (k, v_) in zip(sequential_gauss["N"], sequential_gauss["exec_time"])}

    dat = df_gauss_o3[df_gauss_o3["version"] == v]
    plot_benchmark_over_N(
        dat, f"GaussSeidel_{v}_Nbenchmark_sequentialScale.jpeg", scale_sequential)
    plot_benchmark_over_threads(
        dat, f"GaussSeidel_{v}_threadsbenchmark_sequentialScale.jpeg", scale_sequential)

    dat_single = dat[dat["num_threads"] == 1]
    scale_dat = {k: v for (k, v) in zip(
        dat_single["N"], dat_single["exec_time"])}
    plot_benchmark_over_N(
        dat, f"GaussSeidel_{v}_Nbenchmark.jpeg", scale_dat)
    plot_benchmark_over_threads(
        dat, f"GaussSeidel_{v}_threadbenchmark.jpeg", scale_dat)

    #####################
    dat = df_gauss_withoutO3[df_gauss_withoutO3["version"] == v]

    plot_benchmark_over_N(
        dat, f"GaussSeidel_{v}_NotCompilerOptimized_Nbenchmark_sequentialScale.jpeg", scale_sequential)

    dat_single = dat[dat["num_threads"] == 1]
    scale_dat = {k: v for (k, v) in zip(
        dat_single["N"], dat_single["exec_time"])}
    plot_benchmark_over_N(
        dat, f"GaussSeidel_{v}_NotCompilerOptimized_Nbenchmark.jpeg", scale_dat)
    plot_benchmark_over_threads(
        dat,
        f"GaussSeidel_{v}_NotCompilerOptimized_threadbenchmark_sequentialScale.jpeg",
        scale_sequential)

    plot_benchmark_over_threads(
        dat,
        f"GaussSeidel_{v}_NotCompilerOptimized_threadbenchmark.jpeg",
        scale_dat)
