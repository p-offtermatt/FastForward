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
    continuous_folder = folder_name.joinpath("continuous")
    woflan_folder = folder_name.joinpath("woflan")

    lola_files = benchmark_utils.GetBenchmarkInstancesFromFolder(lola_folder, ".lola")
    continuous_files = benchmark_utils.GetBenchmarkInstancesFromFolder(continuous_folder, ".lola")
    woflan_files = benchmark_utils.GetBenchmarkInstancesFromFolder(woflan_folder, ".pnml")

    
    if not benchmark_utils.EnsureSameFiles(lola_files, continuous_files, "LoLA", "Continuous"):
        exit(1)
    if not benchmark_utils.EnsureSameFiles(woflan_files, continuous_files, "woflan", "Continuous"):
        exit(1)

    # files are identical; to avoid naming confusion, assign new variable
    files = lola_files


    # ensure lola files have an associated formula file, e.g. for file.lola, there should exist file.formula in the same subfolder
    benchmark_utils.EnsureFormulaFilesExist(lola_folder, files)

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

                ff_netpath = os.path.join(continuous_folder, dir, entry + ".lola")
                ff_formulapath = benchmark_utils.GetFormulaFileForNet(ff_netpath)

                
                ff_result = benchmark_utils.call_fastforward("continuous-sound", ff_netpath, "", pruning=False,
                    timeout_time=timeout_time, extra_options="")
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
                lola_result["methodName"] = "lola"

                output_file.write(",\n")
                output_file.write(json.dumps(lola_result))
                output_file.flush()

                woflan_filepath = os.path.join(woflan_folder, dir, entry + ".pnml")

                woflan_result = benchmark_utils.call_woflan(woflan_filepath, timeout_time)
                woflan_result["sampleName"] = entry
                woflan_result["methodName"] = "woflan"

                output_file.write(",\n")
                output_file.write(json.dumps(woflan_result))
                output_file.flush()
            output_file.write("]")
            output_file.flush()