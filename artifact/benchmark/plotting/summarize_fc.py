import argparse

from tabulate import tabulate
import plotting_utils as utils
import matplotlib.pyplot as plt
import numpy as np


def get_woflan_reason(entry):
    if "diagnosisResult" not in entry:
        return "dnf"
    
    diagnosis = entry["diagnosisResult"]

    if "not bounded" in diagnosis:
        return "unbounded"
    
    if "dead transitions" in diagnosis:
        return "dead"

    if "not live" in diagnosis:
        return "nonlive"

    assert False, diagnosis
    

def print_statistics(entries, name):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    for entry in entries:
        if "error" in entry:
            entry["wallTime"] = 120000

    lola_entries = [
        entry for entry in entries if "methodName" in entry and entry["methodName"] == "lola"]
    print(f"Lola runs: {len(lola_entries)}")
    print("----------------------------------------")

    continuous_reach_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
                  == "continuous"]
    print(f"Continuous reachability runs: {len(continuous_reach_entries)}")
    print("----------------------------------------")

    woflan_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
                  == "woflan"]
    print(f"Woflan runs: {len(woflan_entries)}")
    print("----------------------------------------")


    grouped_entries = utils.group_by_name(continuous_reach_entries + lola_entries + woflan_entries)
    
    grouped_tuples = [(name, entry) for name, entry in grouped_entries.items()]

    names_to_conti_entries = utils.group_by_name(continuous_reach_entries)
    names_to_woflan_entries = utils.group_by_name(woflan_entries)

    num_entries = 50
    print(f"----------------Largest {num_entries} entries-------------------------")
    data = []
    for entry in grouped_tuples:
        sample_name = entry[0]
        row = [sample_name]
        # add info about net: num of places and transitions
        row += [names_to_conti_entries[sample_name][0]["numberOfPlaces"]]
        row += [names_to_conti_entries[sample_name][0]["numberOfTransitions"]]
        row += ["yes" if names_to_conti_entries[sample_name][0]["isZBounded"] == True else "no"]
        # row += [("o_star" in names_to_woflan_entries[sample_name][0]["diagnosisResult"]) if "diagnosisResult" in names_to_woflan_entries[sample_name][0] else "dnf"]

        # row += [get_woflan_reason(names_to_woflan_entries[sample_name][0])]

        row += [names_to_conti_entries[sample_name][0]["timeForZBoundedness"]]
        row += [names_to_conti_entries[sample_name][0]["timeForContinuousSoundness"]]
        # add results for tools
        row += utils.get_time_from_entries(entry[1])

        data.append(row)

    headers = ["sampleName", "numPlaces", "numTransitions", "time for z-boundedness", "time for continuous soundness"] + [entry["methodName"] for entry in grouped_tuples[0][1]]

    data.sort(key=lambda entry: sum(entry[-3:]), reverse=True)

    data = data[:num_entries]

    zunbounded = [list(entry) for entry in data if entry[3] == "no"]
    zbounded = [list(entry) for entry in data if entry[3] == "yes"]

    data = [["z-unbounded"]] + zunbounded + [["midrule"], ["continuous-unsound"]] + zbounded

    for i, entry in enumerate(data):
        if len(entry) >= 4:
            del entry[3]

    print(tabulate(data, headers, floatfmt=".1f", tablefmt="plain").replace("120000.0", "dnf"))
    print(np.mean([float(entry[-2]) for entry in data if len(entry) > 3]))
    print(np.mean([float(entry[-1]) for entry in data if len(entry) > 3]))



if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)

    print_statistics(json_obj, name=filepath)
