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
    parser = argparse.ArgumentParser()
    parser.add_argument('benchmark_dir', type=str,
                        help="The directory containing the lola and fastforward subfolders, which should in turn contain the input files, potentially again in subfolders. Subfolders are assumed to be mirrored among the lola and fastforward subfolders.")
    parser.add_argument("-o", "--outputfile", type=str, required=True)

    args = parser.parse_args()

    folder_name = args.benchmark_dir
    output_filepath = args.outputfile

    folder_name = folder_name.rstrip("/")

    folder_name = Path(folder_name).absolute()
    print(folder_name)
    
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
            

    # with open(output_filepath, 'w+') as output_file:
    #     output_file.write("[")
    #     output_file.flush()
    #     first = True
    #     for entry in os.scandir(folder_name):
    #         path = entry.path
    #         print("----" + path + "----")
    #         if path.endswith(".lola"):
    #             ending = ".lola"
    #         elif path.endswith(".xml"):
    #             ending = ".xml"
    #         else:
    #             continue

    #         if not first:
    #             output_file.write(",\n")
    #         first = False

    #         path_prefix = path[:-len(ending)]
    #         folder_prefix, filename = path_prefix.rsplit("/", 1)

    #         command = f"dotnet fastforward/fastforward.dll soundness-reverseTransitions {path_prefix}{ending} -k 1"
    #         process = Popen(command.split(" "), stdout=PIPE,
    #                         stderr=PIPE, preexec_fn=benchmark_utils.limit_virtual_memory)
    #         try:
    #             print(command)
    #             execution_time = time.time()
    #             result, stderr = process.communicate(timeout=timeout_time)
    #             result_obj = json.loads(result)
    #         except CalledProcessError:
    #             execution_time = time.time() - execution_time
    #             process.kill()
    #             result, stderr = process.communicate()

    #             result_obj = {"error": stderr.decode(
    #                 "utf-8").replace("\"", "'")}
    #         except TimeoutExpired:
    #             execution_time = time.time() - execution_time
    #             result_obj = {"error": "timeout"}
    #             process.kill()
    #             result, stderr = process.communicate(timeout=timeout_time)

    #             print("Timeout!")
    #         except json.JSONDecodeError as e:
    #             process.kill()
    #             print("Encountered an error:")
    #             print(e.msg)
    #             print(stderr.decode("utf-8"))
    #             print(result.decode("utf-8"))
    #             result_obj = {"error": repr(
    #                 e) + ", " + stderr.decode("utf-8".replace("\"", "'"))}
    #         result_obj["file"] = path

    #         output_file.write(json.dumps(result_obj))
    #         output_file.flush()

    #     output_file.write("]")
    #     output_file.flush()
