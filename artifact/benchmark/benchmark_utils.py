import argparse
import psutil
import os
import glob
from subprocess import check_output, Popen, PIPE, CalledProcessError, TimeoutExpired
import csv
import time
import json
import sys
import re
import resource
import signal
import contextlib
from shutil import copy, copyfile

MAX_VIRTUAL_MEMORY = (1024  # b -> kb
                      * 1024  # kb -> mb
                      * 1024  # mb -> gb
                      * 8.1)  # <- this many gb


def limit_virtual_memory():
    # method taken from: https://gist.github.com/s3rvac/f97d6cbdfdb15c0a32e7e941f7f4a3fa
    # The tuple below is of the form (soft limit, hard limit). Limit only
    # the soft part so that the limit can be increased later (setting also
    # the hard limit would prevent that).
    # When the limit cannot be changed, setrlimit() raises ValueError.
    resource.setrlimit(resource.RLIMIT_AS,
                       (MAX_VIRTUAL_MEMORY, resource.RLIM_INFINITY))
####


def limit_memory_and_set_sid():
    pass
    # limit_virtual_memory()
    # os.setsid()


def call_fastforward_helper(command, timeout_time):
    process = Popen(command.split(" "), stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)
    try:
        print(command)
        execution_time = time.time()
        result, stderr = process.communicate(timeout=timeout_time)

        process.kill()
        execution_time = time.time() - execution_time

        if result:
            result_obj = json.loads(result)
        else:
            print("empty output!")
            result_obj = {}
    except CalledProcessError:
        execution_time = time.time() - execution_time
        process.kill()
        result, stderr = process.communicate()

        result_obj = {"error": stderr.decode("utf-8").replace("\"", "'")}
    except TimeoutExpired:
        execution_time = time.time() - execution_time
        result_obj = {"error": "timeout"}
        process.kill()
        result, stderr = process.communicate(timeout=timeout_time)

        print("Timeout!")
    except json.JSONDecodeError as e:
        process.kill()
        print("Encountered an error:")
        print(e.msg)
        print(stderr.decode("utf-8"))
        print(result.decode("utf-8"))
        result_obj = {"error": repr(
            e) + ", " + stderr.decode("utf-8".replace("\"", "'"))}

    result_obj["wallTime"] = execution_time * 1000
    return result_obj


def call_fastforward(method_name, lola_file, formula_file, pruning, extra_options, timeout_time):
    command = f"dotnet fastforward/fastforward.dll {method_name} {lola_file} " \
        f"{formula_file} {extra_options}" + \
        (" -p" if pruning else "")
    result_obj = call_fastforward_helper(command, timeout_time)
    return result_obj


def kill(proc_pid):
    process = psutil.Process(proc_pid)
    for proc in process.children(recursive=True):
        proc.kill()
    process.kill()

def call_woflan(net_file, timeout_time):
    prom_home = os.getenv("PROM_HOME")
    if prom_home is None:
        raise FileNotFoundError("PROM_HOME is not set! Make sure the env variable PROM_HOME points to the ProM installation!")
    prom_net_file = prom_home + "/net.pnml"

    copyfile(net_file, prom_net_file)

    command = f"sh ProM_CLI.sh -f call_woflan.java"
    print(command)
    process = Popen(command.split(" "), stdout=PIPE, stderr=PIPE, preexec_fn=limit_virtual_memory, cwd=prom_home)
    result_obj = {}
    try:
        stdout, stderr = process.communicate(timeout=timeout_time)
        result_string = stdout.decode('utf-8')
        woflan_time = re.search(r"Total Woflan time .*\n([0-9\.]*)", result_string, re.MULTILINE).group(1)
        result_obj["wallTime"] = float(woflan_time)

        diagnosis__result = re.search(r"Woflan diagnosis of net .*\n((.*\n)*)End of Woflan diagnosis", result_string, re.MULTILINE).group(1)
        result_obj["diagnosisResult"] = diagnosis__result

        print("Took " + woflan_time + " millis")
        print(diagnosis__result)
    except TimeoutExpired:
        result_obj["error"] = "timeout"
        print("TIMEOUT!")
        kill(process.pid)
    except Exception as e:
        print("Encountered error:")
        print(e)
        result_obj["error"] = str(e)
    

    return result_obj


def call_lola(lola_file, formula_file, timeout_time):
    with contextlib.suppress(FileNotFoundError):
        os.remove("lola.json")
    net = open(f"{lola_file}").read().replace("\n\n", "\n")

    command = f"lola --quiet --json=lola.json --jsoninclude=path --formula={formula_file}"
    print(command)

    process = Popen(command.split(" "), stdin=PIPE, stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)
    result_obj = {}
    try:
        execution_time = time.time()
        result, stderr = process.communicate(
            input=net.encode(), timeout=timeout_time)

        process.kill()

        if os.path.exists("lola.json"):
            result_file = open("lola.json", 'r')
            lola_result = result_file.read()
            result_file.close()
            os.remove("lola.json")
        else:
            print("Lola did not write a result file, probably memout")
            result_obj = {}
            result_obj["error"] = "memout"
            result_obj["lola_stderr"] = stderr.decode('utf-8')
            result_obj["lola_special_commentary"] = "lola did not write a json result"
            result_obj["methodName"] = "Lola"
            result_obj["wallTime"] = timeout_time * 1000
            return result_obj

        execution_time = time.time() - execution_time
        lola_result = json.loads(lola_result)
        result_obj["methodName"] = "Lola"

        result_obj["numberOfPlaces"] = lola_result["net"]["places"]
        result_obj["numberOfTransitions"] = lola_result["net"]["transitions"]

        result_obj["lola_special_commentary"] = lola_result
        result_obj["lola_stderr"] = stderr.decode('utf-8')
        result_obj["wallTime"] = execution_time * 1000

        if(lola_result["call"]["signal"] == "Interrupt" or lola_result["call"]["error"] == "Cannot allocate memory"):
            # interrupt happens when the OutOfMemory killer on Linux kills the process -> out of memory error
            print("Lola ran out of memory...")
            result_obj["error"] = "memout"
            return result_obj

        if(lola_result["call"]["error"] is not None and lola_result["call"]["error"] != ""):
            print("Lola ran into an error: " + lola_result["call"]["error"])
            print("Here is stderr: " + stderr)
            print("Here is stdout: " + result)
            return result_obj

        if "stats" in lola_result["analysis"]:
            result_obj["expandedNodes"] = lola_result["analysis"]["stats"]["states"]
        else:
            result_obj["expandedNodes"] = "not-reported"

        if "result" in lola_result["analysis"]:
            if lola_result["path"] is None:
                result_obj["path"] = "unreachable"
            else:
                result_obj["path"] = ", ".join(lola_result["path"])
        else:
            result_obj["path"] = "not-reported"
    except TimeoutExpired:
        execution_time = time.time() - execution_time
        result_obj = {"error": "timeout"}
        process.kill()
        result, stderr = process.communicate(timeout=timeout_time)
        print("Timeout!")
    except json.JSONDecodeError as e:
        execution_time = time.time() - execution_time
        process.kill()
        print("Encountered an error:")
        print(str(e))
        print("Json was " + result.decode("utf-8"))
        result_obj = {"error": stderr.decode("utf-8") + ", " + str(e)}
    except BaseException as e:
        print("Encountered exception: " + repr(e))
        print("Json was " + result.decode("utf-8"))
        result_obj = {"error": repr(e) + ", " + result.decode("utf-8")}
        process.kill()

    result_obj["wallTime"] = execution_time * 1000
    return result_obj

def CheckFormulaFileExists(net_filepath):
    formula_filepath = GetFormulaFileForNet(net_filepath)
    if not os.path.isfile(formula_filepath):
        raise FileNotFoundError("Netfile " + net_filepath + " does not have an associated .formula file! Make sure the file is in the same folder and has the same name, except for a .formula extension: " + formula_filepath)

def GetFormulaFileForNet(net_filepath):
    root,ext = os.path.splitext(net_filepath)
    assert(ext == ".lola")
    return root + ".formula"

# ensure that the two input dictionaries (string->list of files) contain the same values, and if not, print the difference
# names are needed to nicely print error message
def EnsureSameFiles(dict1_files: 'dict[str, list[str]]', dict2_files: 'dict[str, list[str]]', dict1_name: str, dict2_name: str):
    if(dict2_files != dict1_files):
        print(f"Input instances for {dict1_name} and {dict2_name} are not the same.")
        for dir,entries in dict1_files.items():
            if dir not in dict2_files:
                print(f"{dict2_name} does not have subfolder " + dir)
                return False
            difference = set(entries) - set(dict2_files[dir])
            if len(difference) > 0:
                print(f"Extra files for {dict1_name} in " + dir + ": " + str(difference))
                return False

        
        for dir,entries in dict2_files.items():
            if dir not in dict1_files:
                print(f"{dict1_name} does not have subfolder " + dir)
                return False
            difference = set(entries) - set(dict1_files[dir])
            if len(difference) > 0:
                print(f"Extra files for {dict2_name} in " + dir + ": " + str(difference))
                return False
    return True

def CreateBenchmarkArgparser():
    parser = argparse.ArgumentParser()
    parser.add_argument('benchmark_dir', type=str,
                        help="The directory containing the lola and fastforward subfolders, which should in turn contain the input files, potentially again in subfolders. Subfolders are assumed to be mirrored among the lola and fastforward subfolders.")
    parser.add_argument("-o", "--outputfile", type=str, required=True, 
        help="""The base name for the output file. Should not contain the extension, which will be chosen automatically. Also, one output file will be 
        produced per subdirectory in the fastforward and lola subfolders. For example, if there is a folder lola/unsound,
        then one output file will be called {outputfile}_unsound.json""")

def GetBenchmarkInstancesFromFolder(folderpath, extension):
    print(f"Looking for {extension} files in {folderpath}")
    result = dict()
    for dir,_,files in os.walk(folderpath):
        reldir = os.path.relpath(dir, start=folderpath)
        if len(files) > 0:
            result[reldir] = []
        for file in files:
            if file.endswith(extension):
                result[reldir] += [file]
    return result

def EnsureFormulaFilesExist(folder, net_files):
    for dir, entries in net_files.items():
        for entry in entries:
            net_filepath = os.path.join(folder, dir, entry)
            CheckFormulaFileExists(net_filepath)
