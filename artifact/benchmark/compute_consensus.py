import csv
import pandas
import sys
import json
from tabulate import tabulate
import argparse

def find_consensus(reachables, unreachables):
    if "BFC" in [x[0] for x in reachables] or "MIST" in [x[0] for x in reachables]:
        return "reachable"
    
    if "BFC" in [x[0] for x in unreachables] or "MIST" in [x[0] for x in unreachables]:
        return "unreachable"

    if len(reachables) > len(unreachables):
        return "reachable"

    if len(unreachables) > len(reachables):
        return "unreachable"

    return "unclear"


def group_entries(entries, function):
    group_to_entries = {}
    for entry in entries:
        group_name = function(entry)
        group_to_entries[group_name] = group_to_entries.get(
            group_name, []) + [entry]
    return group_to_entries

def read_json_from_file(filepath):
    with open(filepath, 'r') as json_file:
        print("Reading input from " + json_file.name)
        input_str = json_file.read()
        try:
            json_obj = json.loads(input_str)
        except json.decoder.JSONDecodeError:
            input_str = input_str.replace("\'", "\"")
            try:
                json_obj = json.loads(input_str)
            except json.decoder.JSONDecodeError:
                input_str = input_str + "]"
                json_obj = json.loads(input_str)
    return json_obj

def transform_entry(entry):
    # do stuff
    return entry

def write_json_to_file(filepath, json_obj):
    with open(filepath, 'w+') as output_file:
        json.dump(json_obj, output_file, indent=2)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")
    parser.add_argument("-o", "--outputfile", type=str, required=True)

    args = parser.parse_args()

    tex_filepath = args.outputfile
    collected_data = None
    
    data = []

    # read input
    for filepath in args.datafiles:
        json_obj = read_json_from_file(filepath)
        # special handling for mist benchmarks
        if "bounded" in filepath:
            json_obj = [transform_entry(entry) for entry in json_obj]

        data += json_obj


    grouped_data = {}
    for entry in data:
        cur_list = grouped_data.get(entry["sampleName"], [])
        cur_list += [entry]
        grouped_data[entry["sampleName"]] = cur_list

    # print(grouped_data)

    instances = []
    
    for sample, group in grouped_data.items():
        opinions = [(member["methodName"], member["path"]) for member in group if member.get("error", "") == ""]
        unreachables = [x for x in opinions if x[1] == "unreachable" or x[1] == "terminating"]
        reachables = [x for x in opinions if x[1] != "unreachable" and x[1] != "terminating"]
        instances += [(sample, unreachables, reachables)]
    
    result = []
    for sample, unreachables, reachables in instances:
        if unreachables and reachables:
            print("----")
            print("Dissensus for sample " + sample)
            print("Unreachables: " + ", ".join([x[0] for x in unreachables]))
            print("Reachables : " + ", ".join([x[0] for x in reachables]))
            print("----")
        verdict = find_consensus(reachables, unreachables)
        result.append({"sampleName": sample, "consensus": verdict})

    print("Writing to file " + args.outputfile)
    write_json_to_file(args.outputfile, result)