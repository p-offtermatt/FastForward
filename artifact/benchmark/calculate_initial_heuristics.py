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
import argparse

timeout_time = 60

do_pruning = True
do_no_pruning = False


heuristics = {
    "markingEQ": False,
    "qReachability": True,
    "QMarkingEQGurobi": True,
    "NMarkingEQGurobi": True,
    "syntactic": False,
    "markingEqSyntacticBaseline": False,
    "qReachabilitySyntacticBaseline": False,
    "structuralperplace-q": False,
    "structuralperplace-n": False,
}


def calculate_heuristic(method_name, sample_name, lola_file, formula_file, heuristics_dict: Dict[str, bool], output_file, first):
    for heuristic in [heuristicName for (heuristicName, executeBool) in heuristics_dict.items() if executeBool]:
        print(f"Running {heuristic}")
        if do_pruning:
            print("...with Pruning")
            command = f"dotnet fastforward/fastforward.dll calculate-heuristic {lola_file} " \
                f"{formula_file} -h {heuristic} -p"
            result = benchmark_utils.call_fastforward_helper(
                command, timeout_time)
            result["methodName"] = f"{method_name}_{heuristic}+Pruning"
            result["sampleName"] = sample_name
            output_file.write(("," if not first else "") + json.dumps(result))
            output_file.write("\n")
            output_file.flush()
            first = False


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('benchmark_suites', nargs="+",
                        help="One or more directories that contain the input files.")
    parser.add_argument("-o", "--outputfile", type=str, required=True)

    args = parser.parse_args()

    outfile = args.outputfile

    with open(outfile, 'w+') as output_file:
        output_file.write("[")
        output_file.flush()
        first = True
        for benchmark_suite in args.benchmark_suites:
            benchmark_suite += "/" if not benchmark_suite.endswith("/") else ""
            for filename in os.listdir(benchmark_suite):
                print("---- " + filename + " ----")
                if filename.endswith(".lola"):
                    filename_without_ending = filename[:-len(".lola")]

                    calculate_heuristic(method_name="calculate-heuristics",
                                        sample_name=filename_without_ending,
                                        lola_file=f"{benchmark_suite}{filename_without_ending}.lola",
                                        formula_file=f"{benchmark_suite}{filename_without_ending}.formula",
                                        heuristics_dict=heuristics,
                                        output_file=output_file,
                                        first=first)
                    first = False

        output_file.write("]")
        output_file.flush()

    with open(outfile, "r") as json_file:
        dataframe = pandas.read_json(json_file)
        csv = dataframe.to_csv(outfile + ".csv", index=False)
        # write csv to file
