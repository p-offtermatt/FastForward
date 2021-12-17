import argparse

from tabulate import tabulate
import plotting_utils as utils
import matplotlib.pyplot as plt

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
        print(entry)
        if "error" in entry:
            entry_time = 120000
        else:
            entry_time = entry["wallTime"]
        result_list.append(entry_time)
    return result_list

def print_statistics(entries, name):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    # filter out entries with errors
    # entries = [
    #     entry for entry in entries if "error" not in entry or not entry["error"] == "timeout"]
    # print(f"Non-timeout-runs: {len(entries)}")
    # print("----------------------------------------")

    # entries = [
    #     entry for entry in entries if "error" not in entry or not entry["error"] == "memout"]
    # print(f"Non-memout-runs: {len(entries)}")
    # print("----------------------------------------")

    for entry in entries:
        if "error" in entry:
            entry["wallTime"] = 120000

    reachability_entries = [
        entry for entry in entries if "methodName" in entry and entry["methodName"] == "reachability"]
    print(f"Reachability runs: {len(reachability_entries)}")
    print("----------------------------------------")

    continuous_reach_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
                  == "continuous"]
    print(f"Continuous reachability runs: {len(continuous_reach_entries)}")
    print("----------------------------------------")


    for entry in reachability_entries:
        entry.pop("lola_special_commentary", None)
        entry.pop("path", None)
        entry.pop("expandedNodes", None)
        entry.pop("lola_stderr", None)
        entry.pop("timeForNetParsing", None)
        entry.pop("numberOfTransitionsAfterBackwardPruning", None)
        entry.pop("timesHeuristicCalculated", None)
        entry.pop("timeTakenForwardPruning", None)
        entry.pop("timeTakenBackwardPruning", None)
        entry.pop("timeTakenPruning", None)
        entry.pop("timeInAlgorithm", None)
        entry.pop("timeInHeuristicCalculation", None)
        entry.pop("ExpandedNodes", None)
        entry.pop("timeHeuristicInit", None)
        entry.pop("timeForFormulaParsing", None)

    grouped_entries = group_by_name(continuous_reach_entries + reachability_entries)
    
    grouped_entries_no_errors = [(name, entry) for name, entry in grouped_entries.items()]# if not any("error" in x for x in entry[1])]
    grouped_entries_no_errors.sort(key=lambda tup: sum([entry["wallTime"] for entry in tup[1]]))

    
    print("----------------10 entries-------------------------")
    data = []
    for entry in grouped_entries_no_errors:
        sample_name = entry[0]
        row = [sample_name] + get_time_from_entries(entry[1])
        data.append(row)

    headers = ["sampleName"] + [entry["methodName"] for entry in grouped_entries_no_errors[0][1]]

    for entry in data:
        entry[0] = entry[0].split("-")[0]

    data.sort(key=lambda entry: int(entry[0]))
    print(tabulate(data, headers))

    continuous_data = [entry[1] for entry in data]
    reachability_data = [entry[2] for entry in data]
    x = [entry[0].split("-")[0] for entry in data]
    plt.plot(x,continuous_data, label="continuous")
    plt.plot(x,reachability_data, label="reachability")
    plt.legend()
    plt.title(name)

    print(" ".join([f"({x[i]},{continuous_data[i]/1000})" for i in range(0, len(x))]))
    print(" ".join([f"({x[i]},{reachability_data[i]/1000})" for i in range(0, len(x))]))

    plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)

    print_statistics(json_obj, name=filepath)
