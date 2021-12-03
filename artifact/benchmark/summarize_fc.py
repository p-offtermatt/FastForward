import argparse
import plotting_utils as utils
import numpy
import pandas
from tabulate import tabulate

def group_by_name(entries):
    result = dict()
    for entry in entries:
        normalized_sample_name = utils.strip_common_extensions(entry["sampleName"])
        entries_for_name = result.get(normalized_sample_name, list())
        entries_for_name.append(entry)
        result[normalized_sample_name] = entries_for_name

    # ensure all entrylists are ordered in the same way    
    for entrylist in result.values():
        entrylist.sort(key=lambda entry: entry["methodName"])

    return result

def get_time_from_entries(entry_list):
    result_list = list()
    for entry in entry_list:
        if "error" in entry:
            entry_time = "DNF"
        elif entry["methodName"] == "ff-soundness":
            entry_time = entry["timeForWFIntegerBoundednessCounterexample"] + entry["timeForContinuousSoundness"]
        else:
            entry_time = entry["wallTime"]
        result_list.append(entry_time)
    return result_list

def print_statistics(entries):
    print("Printing statistics...")
    print("----------------------------------------")

    grouped_entries = group_by_name(entries)

    # for entry in lola_entries:
    #     entry.pop("lola_special_commentary", None)
    #     entry.pop("path", None)
    #     entry.pop("methodName", None)
    #     entry.pop("expandedNodes", None)
    #     entry.pop("lola_stderr", None)
    #     entry.pop("timeForNetParsing", None)
    #     entry.pop("numberOfTransitionsAfterBackwardPruning", None)
    #     entry.pop("timesHeuristicCalculated", None)
    #     entry.pop("timeTakenForwardPruning", None)
    #     entry.pop("timeTakenBackwardPruning", None)
    #     entry.pop("timeTakenPruning", None)
    #     entry.pop("timeInAlgorithm", None)
    #     entry.pop("timeInHeuristicCalculation", None)
    #     entry.pop("ExpandedNodes", None)
    #     entry.pop("timeHeuristicInit", None)
    #     entry.pop("timeForFormulaParsing", None)


    grouped_entrylist = [x for x in grouped_entries.items()]


    grouped_entrylist_no_errors = [entry for entry in grouped_entrylist if not any("error" in x for x in entry[1])]
    grouped_entrylist_no_errors.sort(key=lambda tup: sum([entry["wallTime"] for entry in tup[1]]))
    
    print("----------------Entries with timeout-------------------------")
    grouped_entrylist_only_errors = [entry for entry in grouped_entrylist if any("error" in x for x in entry[1])]

    headers = ["sampleName"] + [entry["methodName"] for entry in grouped_entrylist_only_errors[0][1]]
    data = []
    for entry in grouped_entrylist_only_errors:
        sample_name = entry[0]
        row = [sample_name] + get_time_from_entries(entry[1])
        data.append(row)
    print(tabulate(data, headers))

    print("----------------largest entries-------------------------")
    data = []
    for entry in grouped_entrylist_no_errors:
        sample_name = entry[0]
        row = [sample_name] + get_time_from_entries(entry[1])
        data.append(row)
    
    print(tabulate(data, headers))

    # print(tabulate(error_table, headers="keys"))
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    # read input
    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)

    print_statistics(json_obj)
