import csv
import pandas
import sys
import json
from tabulate import tabulate
import math
import matplotlib.pyplot as plt

times_seen = {}

default_time = 600000


def get_method_name_from_sample_name(sampleName):
    # print(sampleName)
    if("." in sampleName):
        sampleName = sampleName.rsplit(".", 1)[1]

    sampleName = sampleName.rsplit("_", 1)[1]
    # print(sampleName)

    return sampleName


def get_benchmark_name_from_sample_name(sampleName: str):
    # print("Before: " + sampleName)
    sampleName = sampleName.rsplit("_", 1)[0]

    if sampleName.endswith(".spec"):
        sampleName = sampleName[:-len(".spec")]

    if "_a-star" in sampleName:
        sampleName = sampleName[:sampleName.index("_a-star")]

    if "_best-first" in sampleName:
        sampleName = sampleName[:sampleName.index("_best-first")]

    # print("After: " + sampleName)
    return sampleName


def identify_shortest_path(entries, benchmark_name):
    fitting_entries = [
        entry for entry in entries if entry["sampleName"] == benchmark_name]

    # unreachable_entries = [entry for entry in fitting_entries if entry.get("path", "") == "unreachable"]
    # error_entries = [entry for entry in fitting_entries if entry.get("error", "") != ""]
    # reachable_entries = [entry for entry in fitting_entries if entry.get("path", "") not in ["unreachable", ""]]

    q_marking_entries = [entry for entry in fitting_entries if "a-star_QMarkingEQGurobi+Pruning" in entry["methodName"]
                         or "a-star+QMarkingEQGurobi+Pruning" in entry["methodName"] or "A* in competitive mode with heuristic QMarkingEQGurobi" in entry["methodName"]]
    entry = q_marking_entries[0]

    result = "unknown" if entry.get(
        "error", "") != "" else "unreachable" if entry["path"] == "unreachable" else entry["path"].split(", ")
    # print(str(result) + " vs " + entry["path"])
    return result


if __name__ == "__main__":

    results = []
    for i in range(1, len(sys.argv), 2):
        heuristic_filename = sys.argv[i]
        complete_result_filename = sys.argv[i+1]

        print("Processing " + heuristic_filename +
              ", " + complete_result_filename)

        with open(heuristic_filename, 'r') as heuristic_file, open(complete_result_filename, 'r') as complete_result_file:
            heuristic_string = heuristic_file.read()
            complete_result_string = complete_result_file.read()

            heuristic_json_obj = None
            complete_json_obj = None
            try:
                heuristic_json_obj = json.loads(heuristic_string)
            except json.decoder.JSONDecodeError:
                heuristic_string = heuristic_string.replace("\'", "\"")
                try:
                    heuristic_json_obj = json.loads(heuristic_string)
                except json.decoder.JSONDecodeError:
                    heuristic_string = heuristic_string + "]"
                    heuristic_json_obj = json.loads(heuristic_string)

            try:
                complete_json_obj = json.loads(complete_result_string)
            except json.decoder.JSONDecodeError:
                complete_result_string = complete_result_string.replace(
                    "\'", "\"")
                try:
                    complete_json_obj = json.loads(complete_result_string)
                except json.decoder.JSONDecodeError:
                    complete_result_string = complete_result_string + "]"
                    complete_json_obj = json.loads(complete_result_string)

            for entry in heuristic_json_obj:
                method_name = entry["methodName"]
                benchmark_name = entry["sampleName"]
                transitionSupport = entry["transitionSupport"]
                shortest_path = identify_shortest_path(
                    complete_json_obj, benchmark_name)
                result = {"benchmark_name": benchmark_name,
                          "method_name": method_name,
                          "transitionSupport": transitionSupport,
                          "shortest_path": shortest_path}
                results.append(result)

    for method in {entry["method_name"] for entry in results}:
        logfile = open(method.replace(" ", "_") + ".txt", "w")
        print(method)
        entries_for_method = [
            entry for entry in results if entry["method_name"] == method]

        timesLarger = 0
        timesSame = 0
        timesSmaller = 0

        heuristic_extra = []
        path_extra = []

        number_of_samples = 0

        for entry in entries_for_method:

            if entry["shortest_path"] == "unreachable":
                continue

            number_of_samples += 1

            transitionSupport = set(entry["transitionSupport"].split(", "))
            shortestPathSupport = set(entry["shortest_path"])

            heuristicExtraTransitions = transitionSupport - shortestPathSupport
            pathExtraTransitions = shortestPathSupport - transitionSupport

            if transitionSupport.issuperset(shortestPathSupport) and transitionSupport != shortestPathSupport:
                timesLarger += 1

            if transitionSupport.issubset(shortestPathSupport) and transitionSupport != shortestPathSupport:
                timesSmaller += 1

            if transitionSupport == shortestPathSupport:
                timesSame += 1


            heuristic_extra.append(heuristicExtraTransitions)
            path_extra.append(pathExtraTransitions)

            logfile.write("Sample Name: " + entry["benchmark_name"] + "; Heuristic Support: " + str(transitionSupport) + 
                        "; Shortest Path Support: " + str(shortestPathSupport) + 
                        "; Heuristic Extra: " + str(heuristic_extra) + 
                        "; Shortest Path Extra: " + str(path_extra) + "\n")

            if transitionSupport == shortestPathSupport:
                print(entry["benchmark_name"])
                print("Total transitions in heuristic: " +
                      str(len(transitionSupport)))
                print("Heuristic has " +
                      str(len(heuristicExtraTransitions)) + " extra transitions!")
                print("Path has " + str(len(pathExtraTransitions)) +
                      " extra transitions!")
                print("%%%%%%%%%%%%%%%%%%%%%%")

        logfile.flush()
        logfile.close()

        print("----------------")

        print(method)

        print("Total samples: " + str(number_of_samples))

        print("Heuristic has smaller support: " + str(timesSmaller))
        print("Heuristic has larger support: " + str(timesLarger))
        print("Times the supports are the same: " + str(timesSame))

        path_extra = [len(x) for x in path_extra]
        heuristic_extra = [len(x) for x in heuristic_extra]

        print("Path extra on average: " + str(sum(path_extra)/len(path_extra)))
        print("Path extra max: " + str(max(path_extra)))
        print("Path extra min: " + str(min(path_extra)))

        print("Heuristic extra on average: " +
              str(sum(heuristic_extra)/len(heuristic_extra)))
        print("Heuristic extra max: " + str(max(heuristic_extra)))
        print("Heuristic extra min: " + str(min(heuristic_extra)))
