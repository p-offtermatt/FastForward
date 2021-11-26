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

import psutil

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


def call_icover(spec_file, timeout):
    command = f"python2 icover/main.py --pre --omega {spec_file} limit"
    print(command)

    process = Popen(command.split(" "), stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)
    result_obj = {}
    try:
        execution_time = time.time()
        result, stderr = process.communicate(timeout=timeout)

        process.kill()

        execution_time = time.time() - execution_time

        result = result.decode("utf-8").strip()
        if("MemoryError" in stderr.decode("utf-8")):
            result_obj["error"] = "memout"
        else:
            last_line = result.splitlines()[-1]
            result_obj["path"] = "unreachable" if last_line == 'Safe' else "reachable" if last_line == "Unsafe" else result
    except TimeoutExpired:
        execution_time = time.time()
        result_obj = {"error": "timeout"}
        process.kill()
        result, stderr = process.communicate(timeout=timeout)
        print("Timeout!")

    result_obj["wallTime"] = execution_time * 1000

    return result_obj


def call_bfc(executable_name, tts_file, prop_file, timeout):
    command = f"{executable_name} {tts_file} -a {prop_file}"
    print(command)

    process = Popen(command.split(" "), stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)
    result_obj = {}
    try:
        execution_time = time.time()
        result, stderr = process.communicate(timeout=timeout)

        process.kill()

        execution_time = time.time() - execution_time

        result_string = result.decode("utf-8")
        if "VERIFICATION FAILED" in result_string:
            witness_path_query = re.search(
                r"counterexample\n([\n \w | \d , \/]*)\nResult:",
                result_string,
                re.MULTILINE | re.IGNORECASE
            )

            if witness_path_query is not None:
                witness_path = witness_path_query.group(1)
            else:
                witness_path_query = re.search(
                    r"FW TACE\n-*\n([^-]*)\n-*", result_string, re.MULTILINE | re.IGNORECASE)
                if witness_path_query is not None:
                    witness_path = witness_path_query.group(1)
                else:
                    witness_path = f"Could not extract path from result {result_string}"
                    print(f"(NONFATAL) ERROR: {witness_path}")

        elif "VERIFICATION SUCCESSFUL" in result_string:
            witness_path = "unreachable"
        elif "MEMOUT" in result_string:
            result_obj["error"] = "memout"
            witness_path = ""
        else:
            print("BFC ran into a problem")
            result_obj["error"] = result_string
            witness_path = ""

        result_obj["path"] = witness_path.replace("\n", ";")
    except TimeoutExpired:
        execution_time = time.time() - execution_time
        result_obj["error"] = "timeout"
        process.kill()
        result, stderr = process.communicate(timeout=timeout)
        print("Timeout!")

    result_obj["wallTime"] = execution_time * 1000
    return result_obj


def call_mist(spec_file, timeout):
    command = f"mist --backward {spec_file}"

    process = Popen(command.split(" "), stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)
    result_obj = {}
    try:
        execution_time = time.time()
        result, stderr = process.communicate(timeout=timeout)

        process.kill()

        execution_time = time.time() - execution_time

        result_string = result.decode("utf-8")
        stderr_string = stderr.decode("utf-8")

        # mist does not seem to consume additional memory apart from reading in the model.
        # if there is a memout, it seems to abort immediately, and if it doesn't, there won't ever be a memout.
        # also, memouts mean mist just returns nothing.
        if result_string == "" or "memory exhausted" in stderr_string:
            result_obj["error"] = "memout"
            witness_path = ""
        else:
            if "backward algorithm concludes unsafe" in result_string:
                witness_path = re.search(
                    r"Run to reach the bad states from the initial marking:\n([\n \w | \d , \/, \[, >]*)\nTotal",
                    result_string,
                    re.MULTILINE | re.IGNORECASE
                ).group(1)
            elif "backward algorithm concludes safe" in result_string:
                witness_path = "unreachable"

                if "Unsafe region is empty or lies outside the invariants or contains some initial states" in result_string:
                    expanded_nodes = 1
                else:
                    # match last occurence of 'Iterations', since the index of the last iteration will be the total number of iterations
                    expanded_nodes = re.search(r"Iteration\s*(\d*)(?!.*Iteration)",
                                               result_string,
                                               re.MULTILINE | re.IGNORECASE | re.DOTALL).group(1)

                result_obj["expandedNodes"] = expanded_nodes
            else:
                result_obj["error"] = result_string
                witness_path = ""

        result_obj["path"] = witness_path
    except TimeoutExpired:
        execution_time = time.time() - execution_time

        result_obj = {"error": "timeout"}
        process.kill()
        result, stderr = process.communicate(timeout=timeout)
        print("Timeout!")

    result_obj["wallTime"] = execution_time * 1000
    return result_obj


def call_petrinizer(command, lola_file, sample_name, timeout):
    print(command)

    process = Popen(command.split(" "), stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)
    result_obj = {}
    result_obj["sampleName"] = sample_name

    try:
        execution_time = time.time()
        result, stderr = process.communicate(timeout=timeout)

        process.kill()

        execution_time = time.time() - execution_time

        result_string = result.decode("utf-8")
        if "may not be satisfied." in result_string:
            result_obj["path"] = "inconclusive"
        elif "is satisfied." in result_string:
            result_obj["path"] = "terminating"
        else:
            print("Petrinizer ran into a problem")
            result_obj["error"] = result_string
    except TimeoutExpired:
        execution_time = time.time() - execution_time
        result_obj["error"] = "timeout"
        process.kill()
        result, stderr = process.communicate(timeout=timeout)
        print("Timeout for " + sample_name)

    result_obj["wallTime"] = execution_time * 1000
    return result_obj


def call_kosaraju(spec_file, timeout, is_coverability):
    command = f"./kreach-artifact/kosaraju -q " + \
        ("-c" if is_coverability else "-r") + f" {spec_file}"
    print(command)

    my_env = os.environ.copy()
    my_env["KOSARAJU_SOLVER"] = "cvc4"
    my_env["PATH"] = "/home/local/USHERBROOKE/offp3001/git/a_star/code/benchmark/kreach-artifact/cvc4:" + my_env["PATH"]

    process = Popen(command.split(" "),
                    env=my_env,
                    stdout=PIPE,
                    stderr=PIPE,
                    preexec_fn=limit_memory_and_set_sid)
    result_obj = {}

    try:
        execution_time = time.time()
        result, stderr = process.communicate(timeout=timeout)

        process.kill()

        execution_time = time.time() - execution_time

        stderr_string = stderr.decode("utf-8")
        result_string = result.decode("utf-8")
        result_lines = result_string.strip().split("\n")
        last_line = result_lines[len(result_lines)-1]

        if last_line in ["Unreachable", "Safe"]:
            result_obj["path"] = "unreachable"
        elif last_line in ["Reachable", "Unsafe"]:
            result_obj["path"] = "reachable"
        elif "out of memory" in stderr_string:
            result_obj["error"] = "memout"
        else:
            result_obj["error"] = "Exception: Could not understand output:\n" + \
                result_string + "\n\n" + stderr.decode("utf-8")
            print("Encountered an error:\n")
            print(result_string)
            print(stderr.decode("utf-8"))
    except TimeoutExpired:
        execution_time = time.time() - execution_time
        result_obj["error"] = "timeout"
        os.killpg(os.getpgid(process.pid), signal.SIGTERM)
        result, stderr = process.communicate(timeout=timeout)
        print("Timeout!")

    result_obj["wallTime"] = execution_time * 1000
    return result_obj
