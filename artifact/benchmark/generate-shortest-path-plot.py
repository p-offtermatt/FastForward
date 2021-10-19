import csv
import pandas
import sys
import json
from tabulate import tabulate
import argparse
import math
import plotting_utils as utils


def group_entries(entries, function):
    group_to_entries = {}
    for entry in entries:
        group_name = function(entry)
        group_to_entries[group_name] = group_to_entries.get(
            group_name, []) + [entry]
    return group_to_entries


TEXFILE_TEMPLATE = r"""
    {###ENTRIES PLACEHOLDER###}
        """

ENTRY_TEMPLATE = r"""
\addplot[only marks, color={col{###TOOL NAME PLACEHOLDER###}}, mark={###TOOL MARK PLACEHOLDER###}] coordinates {{###POINTS PLACEHOLDER###}};
"""

TIME_LIMIT = 600000


def get_entry_from_data(sampleName, data):
    return [x for x in data if x["sampleName"] == sampleName][0]


def unify_name(entry):
    entry["methodName"] = entry["methodName"].replace("A*", "a-star")
    return entry


def write_texfile(tex_filepath,
                  data, tools):

    points_for_methods = {}

    for sample, entries in data.items():
        
        # we know for sure that Dijkstra and A* get shortest paths, so use these as baseline
        baseline_entries = [
            entry for entry in entries if "FastForward_a-star" in entry["methodName"]]

        baseline_lengths = [(entry["methodName"], utils.extract_pathlength_from_entry(
            entry)) for entry in baseline_entries if utils.extract_pathlength_from_entry(
            entry) is not None]

        if len({length for name, length in baseline_lengths if length is not None}) > 1:
            print("Different baseline lengths for " + sample)
            print(baseline_lengths)
            exit(12)

        if not baseline_lengths:
            continue

        baseline_length = baseline_lengths[0][1]

        for entry in [entry for entry in entries if entry["methodName"]]:
            if entry.get("error", "") != "":
                continue
            method_name = entry["methodName"]
            path_length = utils.extract_pathlength_from_entry(entry)
            if path_length is None:
                continue
            cur_list = points_for_methods.get(method_name, [])
            points_for_methods[method_name] = cur_list + \
                [(path_length, baseline_length, entry["sampleName"])]

    result = TEXFILE_TEMPLATE

    print(points_for_methods.keys())

    tex_entries = []
    legend_entries = []
    legend_name_entries = []
    for method_name, pointlist in points_for_methods.items():
        if method_name not in tools:
            continue
        entry = ENTRY_TEMPLATE
        points_entries = []
        for path_length, baseline_length, sampleName in pointlist:
            point_height = path_length-baseline_length
            if point_height < 0:
                continue
            point = f"({baseline_length}, {0 if path_length-baseline_length == 0 else 0.5 if path_length-baseline_length == 1 else math.log(path_length-baseline_length, 2)}) % {sampleName}"
            points_entries.append(point)

        print(method_name)
        print("Times equal: " + str(len([x for x in pointlist if x[0] == x[1]])))
        print("Times total: " + str(len(pointlist)))

        entry = entry.replace(
            "{###POINTS PLACEHOLDER###}", "\n".join(points_entries) + "\n")
        entry = entry.replace("{###TOOL NAME PLACEHOLDER###}", method_name.replace("_", " ")).replace("{###TOOL MARK PLACEHOLDER###}", utils.get_tool_mark(method_name))

        tex_entries.append(entry)

    result = result.replace("{###ENTRIES PLACEHOLDER###}", "\n".join(tex_entries))

    with open(tex_filepath, 'w') as tex_file:
        tex_file.write(result)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")
    parser.add_argument("-o", "--outputfile", type=str, required=True)
    parser.add_argument("-t", "--tools", nargs="+", type=str, required=True, help="Which tools to plot.")

    args = parser.parse_args()

    tex_filepath = args.outputfile
    collected_data = {}

    # read input
    for filepath in args.datafiles:
        json_obj = utils.read_json_from_file(filepath)
        json_obj = [unify_name(entry) for entry in json_obj]
        json_obj = [entry for entry in json_obj if entry.get("error", "") == ""]
        data = group_entries(
            json_obj, lambda entry: entry["sampleName"])
        for method_name in set(data.keys()) | set(collected_data.keys()):
            entry_list = collected_data.get(method_name, [])
            entry_list += data.get(method_name, [])
            collected_data[method_name] = entry_list

    # print("\n".join([str(x) for x in tool1_data]))

    write_texfile(tex_filepath,
                  collected_data, args.tools)
