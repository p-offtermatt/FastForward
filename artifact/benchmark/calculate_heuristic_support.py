import os
import glob
from subprocess import check_output, Popen, PIPE, CalledProcessError, TimeoutExpired
import csv
import time
import json
import sys
import benchmark_utils
import pandas
from typing import Dict

timeout_time = 600

benchmark_suite = sys.argv[1]
outfile = sys.argv[2]

do_pruning = True
do_no_pruning = False

def calculate_heuristic(sample_name, lola_file, formula_file, output_file, first):
        command = f"dotnet pn-astar/a_star.dll calculate-support {lola_file} " \
            f"{formula_file}"
        print(f"Running marking equation over N with pruning")
        result = benchmark_utils.run_csharp_tool(
            command + " N", "marking equation over N", sample_name, timeout_time)
        output_file.write(("," if not first else "") + json.dumps(result))
        output_file.write("\n")
        output_file.flush()
        first=False

print(outfile)

with open(outfile, 'w+') as output_file:
    output_file.write("[")
    output_file.flush()
    benchmark_suite += "/" if not benchmark_suite.endswith("/") else ""
    first = True
    for filename in os.listdir(benchmark_suite):
        print("---- " + filename + " ----")
        if filename.endswith(".lola"):
            filename_without_ending = filename[:-len(".lola")]

            calculate_heuristic(sample_name=filename_without_ending,
                                lola_file=f"{benchmark_suite}{filename_without_ending}.lola",
                                formula_file=f"{benchmark_suite}{filename_without_ending}.formula",
                                output_file=output_file,
                                first=first)
            first = False

    output_file.write("]")
    output_file.flush()

print("Done with main file")

with open(outfile, "r") as json_file:
    dataframe = pandas.read_json(json_file)
    csv = dataframe.to_csv(outfile + ".csv", index=False)
    # write csv to file
