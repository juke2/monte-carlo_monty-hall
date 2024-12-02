import subprocess
import sys
import random
import altair as alt
import pandas as pd
import math
import multiprocessing
import re
from typing import Union

# ITER_MAX = 500000  # value used for testing
ITER_MAX = 20000000
DEFAULT_STEP = 100000


def monty_hall_data_collector(step: int, max: int) -> tuple[list]:
    data_list = []
    for iterations in range(step, max + 1, step):
        subprocess.run(
            f"./src/monty-hall.exe {iterations} {random.randrange(0,32767)}",
            stdout=sys.stdout,
            shell=True,
        )
        with open("./src/output/output_monty_hall_val.txt") as f:
            parsed_vals = re.findall(r"[0-9.]+", f.read())
            switch, no_switch = (float(x) for x in parsed_vals)
            data_list.append((iterations, switch, no_switch))
    iter_count, switch, no_switch = (
        [x[0] for x in data_list],
        [x[1] for x in data_list],
        [x[2] for x in data_list],
    )
    return switch, no_switch, iter_count


def monte_carlo_data_collector_c(
    step: int,
    max: int,
    multi_thread: bool = False,
    twist: bool = False,
    deterministic: bool = False,
) -> list[tuple]:
    data_list = []
    for iterations in range(step, max + 1, step):
        subprocess.run(
            f"./src/monte-carlo.exe {iterations} {random.randrange(0,32767)} {1 if multi_thread else 0} 0 {multiprocessing.cpu_count() if multi_thread else 1} {1 if twist else 0} {1 if deterministic else 0}",
            stdout=sys.stdout,
            shell=True,
        )
        with open("./src/output/output_monte_carlo_val.txt") as f:
            parsed_vals = re.findall(r"[0-9.]+", f.read())
            val, time = (float(x) for x in parsed_vals)
            data_list.append((iterations, val, time))
    iter_count, pi_vals, times = (
        [x[0] for x in data_list],
        [abs(math.pi - x[1]) for x in data_list],
        [x[2] for x in data_list],
    )
    return pi_vals, iter_count, times


def monte_carlo_data_collector_fortran(
    step: int, max: int, multithread: bool = False
) -> tuple[list]:
    """
    Run FORTRAN files, and parse pi values and time datum from output
        Parameters:
            step (int) : Acts as initial value. The amount of iterations to step by.
            max (int) : Maximum possible step value. Step is not guaranteed to reach this value.
            multithread (bool) : Flag that decides whether to run with multithreading or without. Defaults to False.
        Output:
            List of lists, indices in the interior list correspond to each other.
            First list contains calculated pi values.
            Second list contains iteration count.
            Third List contains the time taken for each trial.
    """
    mthread, empty_string = "_multithread", ""
    subprocess.run(
        f"./src/approx_pi{mthread if multithread else empty_string}.exe {max} {step} {multiprocessing.cpu_count()}",
        stdout=sys.stdout,
        shell=True,
    )
    with open(
        f"./src/output/fortran_out{mthread if multithread else empty_string}.txt"
    ) as f:
        data_list = f.read().splitlines()
        pi_vals = [
            abs(math.pi - float(datapoint.split(",")[0])) for datapoint in data_list
        ]
        iterations = [int(datapoint.split(",")[1]) for datapoint in data_list]
        times = [float(datapoint.split(",")[2]) for datapoint in data_list]
        # this isn't particularly efficient, but I don't particularly care

    return pi_vals, iterations, times


def main() -> None:
    time_dict = {}
    val_dict = {}
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_c(
        DEFAULT_STEP, ITER_MAX
    )
    val_dict["C"] = pi_vals
    time_dict["C"] = time_vals
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_c(
        DEFAULT_STEP, ITER_MAX, True
    )
    val_dict["C (Multithreaded)"] = pi_vals
    time_dict["C (Multithreaded)"] = time_vals
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_c(
        DEFAULT_STEP, ITER_MAX, False, True
    )
    val_dict["C (Mersenne Twister)"] = pi_vals
    time_dict["C (Mersenne Twister)"] = time_vals
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_c(
        DEFAULT_STEP, ITER_MAX, True, True
    )
    val_dict["C (Multithreaded, Mersenne Twister)"] = pi_vals
    time_dict["C (Multithreaded, Mersenne Twister)"] = time_vals
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_c(
        DEFAULT_STEP, ITER_MAX, False, False, True
    )
    val_dict["C (Deterministic Approximation)"] = pi_vals
    time_dict["C (Deterministic Approximation)"] = time_vals
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_fortran(
        DEFAULT_STEP, ITER_MAX
    )
    val_dict["FORTRAN"] = pi_vals
    time_dict["FORTRAN"] = time_vals
    pi_vals, iteration_counts, time_vals = monte_carlo_data_collector_fortran(
        DEFAULT_STEP, ITER_MAX, True
    )
    val_dict["FORTRAN (Multithreaded)"] = pi_vals
    time_dict["FORTRAN (Multithreaded)"] = time_vals
    graph_time(time_dict, iteration_counts)
    graph_pi_vals(val_dict, iteration_counts)
    switch_probs, no_switch_probs, iter_count = monty_hall_data_collector(10, 20000)
    graph_monty_hall(switch_probs, no_switch_probs, iter_count)


def graph_time(time_datum: dict[str, list], iteration_counts):
    time_transform = {
        "Source": [key for key, data in time_datum.items() for datapoint in data],
        "Time Taken (ns)": [
            datapoint for data in time_datum.values() for datapoint in data
        ],
        "Number of Iterations": iteration_counts * len(time_datum),
    }
    # print(time_transform)
    time_source = pd.DataFrame(time_transform)
    # print(time_source)

    base_time = (
        alt.Chart(time_source)
        .mark_circle(opacity=0.5)
        .encode(
            alt.X("Number of Iterations").scale(domainMin=0.0),
            alt.Y("Time Taken (ns)").scale(domainMin=0.0),
            color="Source",
        )
    )

    base_time = base_time + base_time.transform_regression(
        "Number of Iterations",
        "Time Taken (ns)",
        groupby=["Source"],
        extent=[0, ITER_MAX],
    ).mark_line(size=4)

    log_time = (
        alt.Chart(time_source)
        .mark_circle(opacity=0.5)
        .encode(
            alt.X("Number of Iterations"),
            alt.Y("Time Taken (ns)").scale(type="log"),
            color="Source",
        )
    )

    # log_time = log_time + log_time.transform_loess(
    #     "Number of Iterations", "Time Taken (ns)", groupby=["Source"]
    # ).mark_line(size=4)

    base_time.save("./images/time_taken_over_iterations.png")
    log_time.save("./images/time_taken_over_iterations_LOG.png")


def monte_carlo_point_data_collector():
    "Not implemented yet."
    pass


def graph_pi_vals(
    pi_datum: dict[str, list],
    iter_count: list[int],
) -> None:
    pi_transform = {
        "Source": [key for key, data in pi_datum.items() for datapoint in data],
        "Accuracy of pi (abs(real - calc))": [
            datapoint for data in pi_datum.values() for datapoint in data
        ],
        "Number of Iterations": iter_count * len(pi_datum),
    }

    pi_source = pd.DataFrame(pi_transform)

    base_pi = (
        alt.Chart(pi_source)
        .mark_circle(opacity=0.5)
        .encode(
            alt.X("Number of Iterations").scale(domainMin=0.0),
            alt.Y("Accuracy of pi (abs(real - calc))").scale(type="log"),
            color="Source",
        )
    )

    base_pi = base_pi + base_pi.transform_regression(
        "Number of Iterations",
        "Accuracy of pi (abs(real - calc))",
        groupby=["Source"],
        extent=[0, max(iter_count)],
        method="exp",
    ).mark_line(size=4)

    base_pi.save("./images/pi_accuracy_over_iterations.png")


def graph_monty_hall(
    switch_probs: list[float], no_switch_probs: list[float], iterations: list[int]
):
    switch_legend, no_switch_legend = (
        "Probability Of Prize When Switching",
        "Probability Of Prize Without Switching",
    )
    monty_transform = {
        "Legend": [switch_legend for x in switch_probs]
        + [no_switch_legend for x in no_switch_probs],
        "Probability": switch_probs + no_switch_probs,
        "Number of Iterations": iterations * 2,
    }
    monty_source = pd.DataFrame(monty_transform)

    base_monty = (
        alt.Chart(monty_source)
        .mark_circle(opacity=0.5)
        .encode(
            alt.X("Number of Iterations").scale(domainMin=0.0),
            alt.Y("Probability"),
            color="Legend",
        )
    )

    base_monty = base_monty + base_monty.transform_regression(
        "Number of Iterations",
        "Probability",
        groupby=["Legend"],
        extent=[0, max(iterations)],
    ).mark_line(size=4)

    base_monty.save("./images/monty_hall.png")


if __name__ == "__main__":
    subprocess.run(
        "gcc src/monte-carlo.c -lm -lpthread -o ./src/monte-carlo.exe",
        stdout=sys.stdout,
        shell=True,
    )
    subprocess.run(
        "gcc src/monty-hall.c -o ./src/monty-hall.exe",
        stdout=sys.stdout,
        shell=True,
    )
    subprocess.run(
        "gfortran src/approx_pi.f90  -o src/approx_pi.exe",
        stdout=sys.stdout,
        shell=True,
    )
    subprocess.run(
        "gfortran src/approx_pi_multithread.f90 -fopenmp -o src/approx_pi_multithread.exe",
        stdout=sys.stdout,
        shell=True,
    )
    main()
