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

timeout_time = 600


def compute_statistics(sample_name, lola_file, formula_file, output_file, args, first):
    command = f"dotnet fastforward/fastforward.dll statistics {lola_file} " \
        f"{formula_file} " + args
    print(f"Computing statistics for net " + sample_name)
    result = benchmark_utils.call_fastforward_helper(command, timeout_time)
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

    parser.add_argument("-args", type=str, required=False, help="Flags to give to the call to FastForward. Should be enclosed in quotes. See documentation of FastForward to see options.")

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
                    formula_name = filename_without_ending + ".formula"
                elif filename.endswith(".xml.tpn"):
                    filename_without_ending = filename[:-len(".xml.tpn")]
                    formula_name = filename_without_ending + ".formula"
                elif filename.endswith(".pnml"):
                    filename_without_ending = filename[:-len(".pnml")]
                    formula_name = filename_without_ending + ".formula"
                elif filename.endswith(".xml"):
                    # Hadara File format, see https://github.com/LoW12/Hadara-AdSimul
                    filename_without_ending = filename[:-len(".xml")]
                    formula_name = filename_without_ending + ".formula"
                else:
                    continue

                compute_statistics(sample_name=filename,
                                   lola_file=f"{benchmark_suite}{filename}",
                                   formula_file=f"{benchmark_suite}{formula_name}",
                                   output_file=output_file,
                                   first=first,
                                   args=args.args if args.args is not None else "")
                first = False

        output_file.write("]")
        output_file.flush()

    print("Done with main file")

    with open(outfile, "r") as json_file:
        dataframe = pandas.read_json(json_file)
        csv = dataframe.to_csv(outfile + ".csv", index=False)
        # write csv to file
