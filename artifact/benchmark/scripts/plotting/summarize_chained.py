import argparse
import os

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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    data = []
    headers = ["N", "continuous", "lola", "woflan"]

    size_data = []

    entries_by_chainnum = dict()

    for filepath in args.datafiles:
        # assumes files are named chained_workflows_NUMBER_.json
        chain_num = os.path.splitext(filepath)[0].split("_")[-2]
        entries = utils.read_json_from_file(filepath)
        entries_by_chainnum[chain_num] = entries_by_chainnum.get(chain_num, []) + entries

    timeouts = dict()
    
    for chain_num, entries in entries_by_chainnum.items():
        for entry in entries:
            if "error" in entry:
                entry["wallTime"] = 120000

        lola_entries = [
        entry for entry in entries if "methodName" in entry and entry["methodName"] == "lola"]

        if any([entry["lola_special_commentary"]["analysis"]["result"] for entry in lola_entries]):
            print("LoLA thinks something is sound; error?")
            print(lola_entries)
            exit(1)
        
        continuous_reach_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
                        == "continuous"]

        woflan_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
                        == "woflan"]

        timeout_woflans = [entry for entry in woflan_entries if entry["wallTime"] == 120000]
        print(len(timeout_woflans))
        timeout_fraction = len(timeout_woflans) / len(woflan_entries)
        timeouts[chain_num] = timeout_fraction


        lola_times = [entry["wallTime"] for entry in lola_entries]
        continuous_reach_times = [entry["wallTime"] for entry in continuous_reach_entries]
        woflan_times = [entry["wallTime"] for entry in woflan_entries]

        sizes = [entry["numberOfPlaces"] + entry["numberOfTransitions"] for entry in continuous_reach_entries]
        size_data += [[int(chain_num), sizes]]


        data += [[chain_num, continuous_reach_times, lola_times, woflan_times]]
    data.sort(key=lambda x: int(x[0]))
    size_data.sort(key=lambda x: x[0])

    for func in [np.mean, np.min, np.max]:
        print("---------------" + func.__name__ + "-----------------")
        print("conti")
        print(" ".join(f"({str(entry[0])},{str(func(entry[1]) / 1000)})" for entry in data))

        print("lola")
        print(" ".join(f"({str(entry[0])},{str(func(entry[2])  / 1000)})" for entry in data))

        print("woflan")
        print(" ".join(f"({str(entry[0])},{str(func(entry[3])  / 1000)})" for entry in data))

        print("sizes")
        print(" ".join(f"({str(entry[0])},{str(func(entry[1]))})" for entry in size_data))


    print(sorted(timeouts.items(), key=lambda x: int(x[0])))

