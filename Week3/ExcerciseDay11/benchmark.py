
import re
import pathlib
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def load_data():
    data_path = pathlib.Path("./outputs")
    pattern = re.compile(
        r"(v\d+)\_(O\d+)\_(only|wtihotheroptimizations)\_(MKS|MKD)\_.txt")
    data = []
    for file in data_path.iterdir():
        f_posix = file.relative_to("./outputs").as_posix()
        match = pattern.match(f_posix)
        if match:
            v = match.group(1)
            opt = match.group(2)
            fully_optimized = (match.group(3) != "only")
            precision = match.group(4)
            with open(file, "r") as f:
                file_context = f.readlines()

            cpu_time_line = file_context[5].lower()
            wall_time_line = file_context[6].lower()
            cpu_time = float(
                re.sub("seconds", "", re.sub("cpu time:", "", cpu_time_line)))
            wall_time = float(re.sub("seconds", "", re.sub(
                "wall clock:", "", wall_time_line)))
            data.append({"version": v, "OptLvl": opt,
                         "AdditionalOpt": fully_optimized, 
                         "Precision": precision, "cpu_time":cpu_time, "wall_time":wall_time})
    return pd.DataFrame.from_dict(data)


if __name__ == "__main__":
    data = load_data()
    data.to_csv("diffusion_benchmarks.csv")
    print("The total average cpu_time/time_step is:", data["cpu_time"].mean() / 100)
