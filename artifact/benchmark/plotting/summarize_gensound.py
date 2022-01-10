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
    parser.add_argument('datafile')

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    data = []
    headers = ["c", "continuous", "lola", "woflan"]

    size_data = []

    entries = utils.read_json_from_file(args.datafile)

    for entry in entries:
        if "error" in entry:
            entry["wallTime"] = 120000

    grouped_entries = utils.group_by_name(entries)

    data = []

    for samplename, entry in grouped_entries.items():
        lola_entry = [x for x in entry if x["methodName"] == "lola"][0]
        woflan_entry = [x for x in entry if x["methodName"] == "woflan"][0]
        conti_entry = [x for x in entry if x["methodName"] == "continuous"][0]
        data += [samplename, conti_entry["wallTime"], lola_entry["wallTime"], woflan_entry["wallTime"]]

    data.sort(key=lambda x: int(x[0]))
    size_data.sort(key=lambda x: x[0])
    print("conti")
    print(" ".join(f"({str(entry[0])},{str(np.mean(entry[1]) / 1000)})" for entry in data))

    print("lola")
    print(" ".join(f"({str(entry[0])},{str(np.mean(entry[2])  / 1000)})" for entry in data))

    print("woflan")
    print(" ".join(f"({str(entry[0])},{str(np.mean(entry[3])  / 1000)})" for entry in data))


        

