import csv
import pandas
import sys
import json
from tabulate import tabulate
import argparse
import plotting_utils as utils

TIME_LIMIT = 60000

def group_entries(entries, function):
    group_to_entries = {}
    for entry in entries:
        group_name = function(entry)
        group_to_entries[group_name] = group_to_entries.get(
            group_name, []) + [entry]
    return group_to_entries

def get_entry_from_data(sampleName, data):
    return [x for x in data if x["sampleName"] == sampleName][0]


def entry_satisfies_consensus(entry, consensus_entries):
    return entry.get("error", "") != "" or (entry.get("path") == "unreachable" and consensus_entries[entry["sampleName"]] == "unreachable") or (entry.get("path") != "unreachable" and consensus_entries[entry["sampleName"]] != "unreachable") or consensus_entries[entry["sampleName"]] == "unclear"


def write_texfile(filepath, tool1_data, tool2_data):
    points = []

    i = 0
    benchmark_names = {name for name in [x["sampleName"] for x in tool1_data] + [x["sampleName"] for x in tool2_data]}

    for benchmark_name in benchmark_names:
        i += 1
        try:
            entry1 = [x for x in tool1_data if x["sampleName"]
                      == benchmark_name][0]
        except IndexError:
            print(f"{benchmark_name} not in tool 1's data!")
            continue

        try:
            entry2 = [x for x in tool2_data if
                      x["sampleName"] == benchmark_name][0]
        except IndexError:
            print(f"{benchmark_name} not in tool 2's data!")
            continue

        time1 = None
        if entry1.get("error", "") != "":
            time1 = TIME_LIMIT
        else:
            time1 = entry1["wallTime"]

        time2 = None
        if entry2.get("error", "") != "":
            time2 = TIME_LIMIT
        else:
            time2 = entry2["wallTime"]

        points.append(
                (max(time1, 100), max(time2, 100), benchmark_name))
    result = r"\addplot[only marks, blue, mark size=1.5pt, mark={x}] coordinates {" + "\n"
    result += "\n".join([f"({point[0]}, {point[1]}) % {point[2]}" for point in points])
    result += "\n" + r"};"

    with open(filepath, 'w') as tex_file:
        print("Writing result to " + filepath)
        tex_file.write(result)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")
    parser.add_argument("-o", "--outputfile", type=str, required=True)

    parser.add_argument("-t1", "--tool1", type=str, required=True,
                        help="the name of the first tool to compare")
    parser.add_argument("-t2", "--tool2", type=str, required=True,
                        help="the name of the second tool to compare")

    args = parser.parse_args()

    tex_filepath = args.outputfile
    collected_data = {}

    # read input
    for filepath in args.datafiles:
        json_obj = utils.read_json_from_file(filepath)
        data = group_entries(
            json_obj, lambda entry: entry["methodName"])
        for method_name in set(data.keys()) | set(collected_data.keys()):
            entry_list = collected_data.get(method_name, [])
            entry_list += data.get(method_name, [])
            collected_data[method_name] = entry_list

    print(collected_data.keys())

    tool1_data = collected_data.get(args.tool1, None)
    if tool1_data is None:
        print("Did not find data for tool1: " + args.tool1)
        print("Aborting...")
        sys.exit(12)

    tool2_data = collected_data.get(args.tool2, None)
    if tool2_data is None:
        print("Did not find data for tool2: " + args.tool2)
        print("Aborting...")
        sys.exit(13)

    # print("\n".join([str(x) for x in tool1_data]))

    write_texfile(tex_filepath,
                  tool1_data, tool2_data)
