import argparse
import json
import os
import sys
from subprocess import Popen, PIPE, CalledProcessError, TimeoutExpired
import time
import benchmark_utils
from pathlib import Path

timeout_time = 120

def CheckFormulaFileExists(net_filepath):
    formula_filepath = GetFormulaFileForNet(net_filepath)
    if not os.path.isfile(formula_filepath):
        raise FileNotFoundError("Netfile " + net_filepath + " does not have an associated .formula file! Make sure the file is in the same folder and has the same name, except for a .formula extension: " + formula_filepath)

def GetFormulaFileForNet(net_filepath):
    root,ext = os.path.splitext(net_filepath)
    assert(ext == ".lola")
    return root + ".formula"


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('benchmark_dir', type=str,
                        help="The directory containing the lola and fastforward subfolders, which should in turn contain the input files, potentially again in subfolders. Subfolders are assumed to be mirrored among the lola and fastforward subfolders.")
    parser.add_argument("-o", "--outputfile", type=str, required=True, 
        help="""The base name for the output file. Should not contain the extension, which will be chosen automatically. Also, one output file will be 
        produced per subdirectory in the fastforward and lola subfolders. For example, if there is a folder lola/unsound,
        then one output file will be called {outputfile}_unsound.json""")

    args = parser.parse_args()

    folder_name = args.benchmark_dir
    output_filepath = args.outputfile

    folder_name = folder_name.rstrip("/")

    folder_name = Path(folder_name).absolute()
    print(folder_name)
    
    # get instances in lola and fastforward subdirectories
    lola_folder = folder_name.joinpath("lola")
    ff_folder = folder_name.joinpath("fastforward")
    print(f"Looking for LoLA inputs in {lola_folder}")

    lola_files = dict()
    for dir,_,files in os.walk(lola_folder):
        reldir = os.path.relpath(dir, start=lola_folder)
        if len(files) > 0:
            lola_files[reldir] = []
        for file in files:
            if file.endswith(".lola"):
                lola_files[reldir] += [file]

    print(f"Looking for FastForward inputs in {ff_folder}")

    ff_files = dict()
    for dir,_,files in os.walk(ff_folder):
        reldir = os.path.relpath(dir, start=ff_folder)
        if len(files) > 0:
            ff_files[reldir] = []
        for file in files:
            if file.endswith(".lola"):
                ff_files[reldir] += [file]
    
    # ensure that both directories contain the same instances, in this case .lola files
    if(ff_files != lola_files):
        print("Input instances for lola and fastforward are not the same.")
        for dir,entries in lola_files.items():
            if dir not in ff_files:
                print("Fastforward does not have subfolder " + dir)
                continue
            difference = set(entries) - set(ff_files[dir])
            if len(difference) > 0:
                print("Extra files for LoLA in " + dir + ": " + str(difference))

        
        for dir,entries in ff_files.items():
            if dir not in lola_files:
                print("Fastforward does not have subfolder " + dir)
                continue
            difference = set(entries) - set(lola_files[dir])
            if len(difference) > 0:
                print("Extra files for FastForward in " + dir + ": " + str(difference))
    
    # ff_files and lola_files are identical; to avoid naming confusion, assign new variable
    files = lola_files

    # ensure all files have an associated formula file, e.g. for file.lola, there should exist file.formula in the same subfolder
    for dir, entries in files.items():
        for entry in entries:
            net_filepath = os.path.join(ff_folder, dir, entry)
            CheckFormulaFileExists(net_filepath)

            net_filepath = os.path.join(lola_folder, dir, entry)
            CheckFormulaFileExists(net_filepath)

    for dir, entries in files.items():
        # write result file for this directory of instances
        with open(output_filepath, 'w') as output_file:
            output_file.write("[")
            output_file.flush()
            first = True
            for entry in entries:
                print(f"---- {dir}/{entry} ----")


                benchmark_suite = dir
                instance_name = entry

                ff_netpath = os.path.join(ff_folder, dir, entry)
                ff_formulapath = GetFormulaFileForNet(ff_netpath)

                
                ff_result = benchmark_utils.call_fastforward("calculate-heuristic", ff_netpath, ff_formulapath, pruning=False,
                    extra_options="-h qReachability", timeout_time=timeout_time)
                ff_result["sampleName"] = entry
                ff_result["methodName"] = "continuous"

                if not first:
                    output_file.write(",\n")
                first = False
                output_file.write(json.dumps(ff_result))
                output_file.flush()

                lola_filepath = os.path.join(lola_folder, dir, entry)
                lola_formulapath = GetFormulaFileForNet(lola_filepath)

                lola_result = benchmark_utils.call_lola(lola_filepath, lola_formulapath, timeout_time)
                lola_result["sampleName"] = entry
                lola_result["methodName"] = "reachability"

                output_file.write(",\n")
                output_file.write(json.dumps(lola_result))
                output_file.flush()
            output_file.write("]")
            output_file.flush()