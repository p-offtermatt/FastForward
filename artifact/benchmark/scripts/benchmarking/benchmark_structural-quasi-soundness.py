import argparse
import json
import os
import sys
from subprocess import Popen, PIPE, CalledProcessError, TimeoutExpired
import time
import benchmark_utils
from pathlib import Path

timeout_time = 120


if __name__ == "__main__":
    
    parser = benchmark_utils.CreateBenchmarkArgparser()
    args = parser.parse_args()

    folder_name = args.benchmark_dir
    output_filepath = args.outputfile

    folder_name = folder_name.rstrip("/")

    folder_name = Path(folder_name).absolute()
    
    # get instances in lola and fastforward subdirectories
    lola_folder = folder_name.joinpath("lola")
    ff_folder = folder_name.joinpath("continuous")
    print(f"Looking for LoLA inputs in {lola_folder}")

    lola_files = benchmark_utils.GetBenchmarkInstancesFromFolder(lola_folder, ".lola")

    print(f"Looking for Continuous inputs in {ff_folder}")

    ff_files = benchmark_utils.GetBenchmarkInstancesFromFolder(ff_folder, ".lola")
    
    if not benchmark_utils.EnsureSameFiles(lola_files, ff_files, "LoLA", "Continuous"):
        exit(1)
    
    # ff_files and lola_files are identical; to avoid naming confusion, assign new variable
    files = lola_files

    # ensure all files have an associated formula file, e.g. for file.lola, there should exist file.formula in the same subfolder
    benchmark_utils.EnsureFormulaFilesExist(lola_folder, files)
    benchmark_utils.EnsureFormulaFilesExist(ff_folder, files)

    for dir, entries in files.items():
        # write result file for this directory of instances
        with open(output_filepath + "_" + dir + ".json", 'w') as output_file:
            output_file.write("[")
            output_file.flush()
            first = True
            for entry in entries:
                print(f"---- {dir}/{entry} ----")


                benchmark_suite = dir
                instance_name = entry

                ff_netpath = os.path.join(ff_folder, dir, entry + ".lola")
                ff_formulapath = benchmark_utils.GetFormulaFileForNet(ff_netpath)

                
                ff_result = benchmark_utils.call_fastforward("calculate-heuristic", ff_netpath, ff_formulapath, pruning=False,
                    extra_options="-h qReachability", timeout_time=timeout_time)
                ff_result["sampleName"] = entry
                ff_result["methodName"] = "continuous"

                if not first:
                    output_file.write(",\n")
                first = False
                output_file.write(json.dumps(ff_result))
                output_file.flush()

                lola_filepath = os.path.join(lola_folder, dir, entry + ".lola")
                lola_formulapath = benchmark_utils.GetFormulaFileForNet(lola_filepath)

                lola_result = benchmark_utils.call_lola(lola_filepath, lola_formulapath, timeout_time)
                lola_result["sampleName"] = entry
                lola_result["methodName"] = "reachability"

                output_file.write(",\n")
                output_file.write(json.dumps(lola_result))
                output_file.flush()
            output_file.write("]")
            output_file.flush()