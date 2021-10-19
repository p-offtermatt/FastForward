import csv
import pandas
import sys
import json
from tabulate import tabulate
import argparse
import plotting_utils as utils


def group_entries(entries, function):
    group_to_entries = {}
    for entry in entries:
        group_name = function(entry)
        group_to_entries[group_name] = group_to_entries.get(
            group_name, []) + [entry]
    return group_to_entries


TEXFILE_TEMPLATE = r"""
      {###PLOT PLACEHOLDER###}
        """

ENTRY_TEMPLATE = r"""
%% {###TOOL NAME  PLACEHOLDER###}
\toolplot{{###TOOL NAME###}}{{###SYMBOL PLACEHOLDER###}}{{###POINTS PLACEHOLDER###}};
"""

TIME_LIMIT = 60000
LOGSCALE_ZERO = 0.6

def pointlist_to_summed_list(pointList, logscale):
    pointList = [x for x in pointList]

    if len(pointList) == 0:
        return []

    result = []

    smaller_entries = 0
    previous_smaller_entries = -1
    i = 100
    while True:
        if i > TIME_LIMIT:
            result.append((TIME_LIMIT, smaller_entries))
            return result

        # count how many entries are lower than current i
        smaller_entries = len(
            [entry for entry in pointList if entry <= i])
        if logscale and smaller_entries == 0:
            smaller_entries = LOGSCALE_ZERO 
        if smaller_entries != previous_smaller_entries:
            result.append((i, smaller_entries))

        previous_smaller_entries = smaller_entries
        i *= 2

def unify_name(entry):
    entry["methodName"] = entry["methodName"].replace("A*", "a-star")
    entry["methodName"] = entry["methodName"].replace("_", " ")
    return entry


def unify_namelist(namelist):
    result = [x.replace("_", " ") for x in namelist]
    return result


def write_texfile(filepath, collected_data, logscale):

    # write output
    with open(filepath, 'w') as tex_file:

        tex_entries = []

        tool_number = 0
        for method_name, entries in collected_data.items():

            entries = [entry for entry in entries if entry.get(
                "error", "") == "" and entry.get("path", "") != "inconclusive"]

            tool_number += 1
            points = [obj["wallTime"]
                      for obj in entries if obj.get("error", "") == ""]
            points = pointlist_to_summed_list(points, logscale)

            print(method_name)
            print(points)

            if len(points) == 0:
                points = [(100, (0 if not logscale else LOGSCALE_ZERO)), (TIME_LIMIT, (0 if not logscale else LOGSCALE_ZERO))]

            entry = ENTRY_TEMPLATE.replace(
                "{###TOOL NAME  PLACEHOLDER###}", method_name)
            entry = entry.replace("{###TOOL NAME###}", method_name)
            entry = entry.replace(
                "{###POINTS PLACEHOLDER###}", "".join([str(x) for x in points]))
            entry = entry.replace(
                "{###SYMBOL PLACEHOLDER###}", utils.get_tool_mark(method_name))

            tex_entries.append(entry)


        result = TEXFILE_TEMPLATE.replace(
            "{###PLOT PLACEHOLDER###}", "\n".join(tex_entries))

        if logscale:
            tex_file.write(f"% logscale: the value {LOGSCALE_ZERO} represents zero, since log(0) is problematic")
        tex_file.write(result)
        print("Wrote output to " + str(tex_file.name))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")
    parser.add_argument("-o", "--outputfile", type=str, required=True)
    parser.add_argument("-log", "--logscale", action='store_true', required=False, help="If supplied, sets values of '0' to a small value instead (e.g. 0.6). Useful to plot data in a logarithmic scale, since log(0) is troublesome.")


    args = parser.parse_args()

    tex_filepath = args.outputfile
    collected_data = None

    # read input
    for filepath in args.datafiles:
        json_obj = utils.read_json_from_file(filepath)
        json_obj = [unify_name(entry) for entry in json_obj]
        data = group_entries(json_obj, lambda entry: entry["methodName"])
        if collected_data is None:
            collected_data = data
            continue
        for method_name in set(data.keys()) | set(collected_data.keys()):
            collected_data[method_name] = data.get(method_name, []) + \
                collected_data.get(method_name, [])

    write_texfile(tex_filepath,
                    collected_data, args.logscale)
