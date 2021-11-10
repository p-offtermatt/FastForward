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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('benchmark_suites', nargs="+",
                        help="One or more directories that contain the input files.")
    parser.add_argument("-o", "--outdir", type=str, required=True)

    args = parser.parse_args()

    for benchmark_suite in args.benchmark_suites:
        benchmark_suite += "/" if not benchmark_suite.endswith("/") else ""
        for filename in os.listdir(benchmark_suite):
            print("---- " + filename + " ----")
            if filename.endswith(".lola"):
                filename_without_ending = filename[:-len(".lola")]
            elif filename.endswith(".xml.tpn"):
                filename_without_ending = filename[:-len(".xml.tpn")]
            elif filename.endswith(".pnml"):
                filename_without_ending = filename[:-len(".pnml")]
            elif filename.endswith(".xml"):
                # Hadara File format, see https://github.com/LoW12/Hadara-AdSimul
                filename_without_ending = filename[:-len(".xml")]
            else:
                continue

            command = f"dotnet fastforward/fastforward.dll translate-wf {benchmark_suite}{filename} -o {args.outdir}{filename_without_ending}"
            benchmark_utils.call_fastforward_helper(command, timeout_time)

    print("Done!")
