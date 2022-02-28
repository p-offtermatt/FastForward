import argparse
import json
import os
import sys
from subprocess import Popen, PIPE, CalledProcessError, TimeoutExpired
import time
import benchmark_utils
from pathlib import Path

timeout_time = 120

def get_filename_for_numbers(k, c):
    """
    c is the number for which special behaviour occurs in the instance (e.g. sound for all numbers up to, excluding, c),
    and k is the number for which the property is checked.
    """
    entry_naming_template = "{C}-unsound_{K}-check"
    return entry_naming_template.replace("{C}", str(c)).replace("{K}", str(k))


if __name__ == "__main__":
    parser = benchmark_utils.CreateBenchmarkArgparser()
    args = parser.parse_args()

    folder_name = args.benchmark_dir
    output_filepath = args.outputfile
    timeout_time = args.timeout

    folder_name = folder_name.rstrip("/")

    folder_name = Path(folder_name).absolute()
    
    # get instances in lola and fastforward subdirectories
    # lola_folder = folder_name.joinpath("lola")
    # continuous_folder = folder_name.joinpath("continuous")
    woflan_folder = folder_name.joinpath("woflan")

    # lola_files = benchmark_utils.GetBenchmarkInstancesFromFolder(lola_folder, ".lola")
    # continuous_files = benchmark_utils.GetBenchmarkInstancesFromFolder(continuous_folder, ".lola")
    woflan_files = benchmark_utils.GetBenchmarkInstancesFromFolder(woflan_folder, ".tpn")

    
    # if not benchmark_utils.EnsureSameFiles(lola_files, continuous_files, "LoLA", "Continuous"):
    #     exit(1)
    # if not benchmark_utils.EnsureSameFiles(woflan_files, continuous_files, "woflan", "Continuous"):
    #     exit(1)

    # files are identical; to avoid naming confusion, assign new variable
    files = woflan_files
    # ensure lola files have an associated formula file, e.g. for file.lola, there should exist file.formula in the same subfolder
    # benchmark_utils.EnsureFormulaFilesExist(lola_folder, files)

    for dir, entries in files.items():
        files[dir] = sorted(set([entry.split("-")[0] for entry in entries]), key=lambda x: int(x))

    for dir, entries in files.items():
        # write result file for this directory of instances
        with open(output_filepath + "_" + dir + ".json", 'w') as output_file:
            output_file.write("[")
            output_file.flush()
            first = True
            for c in entries:
                print(f"---- {dir}/{c} ----")
                c = int(c)


                benchmark_suite = dir

                # ff_netpath = os.path.join(continuous_folder, dir, get_filename_for_numbers(1, c) + ".lola")
                # ff_formulapath = benchmark_utils.GetFormulaFileForNet(ff_netpath)

                
                # ff_result = benchmark_utils.call_fastforward("continuous-sound", ff_netpath, "", pruning=False,
                #     timeout_time=timeout_time, extra_options="")
                # ff_result["sampleName"] = c
                # ff_result["methodName"] = "continuous"

                # if not first:
                #     output_file.write(",\n")
                # first = False
                # output_file.write(json.dumps(ff_result))
                # output_file.flush()

                # lola_result = dict()
                # lola_result["sampleName"] = c                    
                # lola_result["methodName"] = "lola"
                # for k in range(1, c+1):
                #     filename = get_filename_for_numbers(k, c)
                #     lola_filepath = os.path.join(lola_folder, dir, filename + ".lola")
                #     lola_formulapath = benchmark_utils.GetFormulaFileForNet(lola_filepath)
                #     remaining_timeout = timeout_time - (lola_result.get("wallTime",0)/1000)
                #     print("Remaining timeout: " + str(remaining_timeout))
                #     k_result = benchmark_utils.call_lola(lola_filepath, lola_formulapath, remaining_timeout)
                #     if "error" in k_result:
                #         lola_result["error"] = k_result["error"]
                #         break
                #     lola_result["wallTime"] = lola_result.get("wallTime", 0) + k_result["wallTime"]
                #     lola_result[k] = k_result
                #     if k == c:
                #         lola_result["result"] = "successful"

                # output_file.write(",\n")
                # output_file.write(json.dumps(lola_result))
                # output_file.flush()


                woflan_result = dict()
                woflan_result["methodName"] = "woflan"
                woflan_result["sampleName"] = c
                for k in range(1, c+1):
                    filename = get_filename_for_numbers(k, c)
                    woflan_filepath = os.path.join(woflan_folder, dir, filename + ".tpn")

                    remaining_timeout = timeout_time - (woflan_result.get("wallTime",0)/1000)
                    print("Remaining timeout: " + str(remaining_timeout))
                    k_result = benchmark_utils.call_woflan(woflan_filepath, remaining_timeout)
                    if "error" in k_result:
                        woflan_result["error"] = k_result["error"]
                        break
                    woflan_result["wallTime"] = woflan_result.get("wallTime", 0) + k_result["wallTime"]
                    woflan_result[k] = k_result
                    if k == c:
                        woflan_result["result"] = "successful"

                output_file.write(",\n")
                output_file.write(json.dumps(woflan_result))
                output_file.flush()
            output_file.write("]")
            output_file.flush()