import csv
import pandas
import sys
import json
from tabulate import tabulate
import math
import argparse
import plotting_utils as utils

times_seen = {}

default_time = 600000


def identify_shortest_path(entries, benchmark_name):
    fitting_entries = [
        entry for entry in entries if entry["sampleName"] == benchmark_name]

    a_star_entries = [
        entry for entry in fitting_entries if "FastForward" in entry["methodName"] and ("QMarkingEQGurobi" in entry["methodName"] or "zero" in entry["methodName"]) and "GBFS" not in entry["methodName"]]
    # print(benchmark_name, a_star_entries)
    a_star_entries = [entry for entry in a_star_entries if entry.get("error", "") == ""]
    entry = a_star_entries[0] if len(a_star_entries) > 0 else {"error": "not found"}

    result = "unknown" if entry.get(
        "error", "") != "" else "unreachable" if entry["path"] == "unreachable" else len(entry["path"].split(", "))
    return result


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('path_files', nargs="+",
                        help="One or more files that contain the results on shortest paths.")
    parser.add_argument('-u', '--heuristic_files', nargs="+",
                        help="One or more files that contain the initial values of heuristics for each instance.", required=True)
    parser.add_argument("-o", "--outputfile", type=str, required=True)

    args = parser.parse_args()

    heuristic_files = args.heuristic_files
    path_files = args.path_files

    results = []

    all_heuristic_data = []


    for heuristic_filename in heuristic_files:
        print("Reading file " + heuristic_filename)
        data = utils.read_json_from_file(heuristic_filename)
        all_heuristic_data += data
        print(f"Read {len({entry['sampleName'] for entry in data})} entries")

    all_path_data = []

    for path_filename in path_files:
        print("Reading file " + path_filename)
        path_data = utils.read_json_from_file(path_filename)
        all_path_data += path_data
        f"Read {len({entry['sampleName'] for entry in path_data})} entries"


    for entry in all_heuristic_data:
        method_name = entry["methodName"]
        benchmark_name = entry["sampleName"]
        if entry.get("heuristic", "") == "" or entry.get("error", "") != "":
            continue
        heuristic = entry["heuristic"]
        shortest_path = identify_shortest_path(
            all_path_data, benchmark_name)
        result = {"benchmark_name": benchmark_name,
                    "method_name": method_name,
                    "heuristic": heuristic,
                    "shortest_path": shortest_path}
        results.append(result)

    methods_to_style = {
        "calculate-heuristics_QMarkingEQGurobi+Pruning": ("diamond*", 2, "colQeq"),
        "calculate-heuristics_qReachability+Pruning": ("x", 2, "colCont"),
        "calculate-heuristics_NMarkingEQGurobi+Pruning": ("*", 2, "colNeq"),
    }


    maximum_value = max([int(entry["shortest_path"])
                         for entry in results if entry["shortest_path"] not in ["unreachable", "unknown"]])

    times_equal = {
        "calculate-heuristics_QMarkingEQGurobi+Pruning": 0,
        "calculate-heuristics_syntactic+Pruning": 0,
        "calculate-heuristics_NMarkingEQGurobi+Pruning": 0,
        "calculate-heuristics_qReachability+Pruning": 0
    }

    entries = []

    for method in methods_to_style.keys():
        print(method)
        markerstyle, size, color = methods_to_style[method]
        entries_for_method = [
            entry for entry in results if entry["method_name"] == method]
        print(f"{len(entries_for_method)} entries for heuristic")
        differences = [(entry["shortest_path"], entry["heuristic"])
                       for entry in entries_for_method if entry["shortest_path"] not in ["unreachable", "unknown"]]
        print(f"{len(differences)} points to compare with shortest path")
        
        if len(differences) == 0:
            continue

        times_equal[method] = len([(shortest_path, heuristic) for (shortest_path, heuristic) in differences if
                                   shortest_path == heuristic])

        entry = rf"\addplot[only marks, mark={markerstyle}, color={color}, mark size={size} pt] plot coordinates" + " { "
        entry += " ".join(f"({shortest_path}, {round(heuristic)})" for (shortest_path, heuristic) in differences)
        entry += " };"

        entries.append(entry)

    with open(args.outputfile, 'w') as outputfile:
        outputfile.write("\n".join(entries))
        print(f"Wrote output to {outputfile.name}")
