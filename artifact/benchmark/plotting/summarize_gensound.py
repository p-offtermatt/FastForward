import argparse
import os

from tabulate import tabulate
import plotting_utils as utils
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
    headers = ["c", "continuous", "lola", "woflan"]

    size_data = []

    entries = []
    
    for file in args.datafiles:
        entries += utils.read_json_from_file(file)

    for entry in entries:
        if "error" in entry:
            entry["wallTime"] = 120000

    grouped_entries = utils.group_by_name(entries)

    data = []

    for samplename, entry in grouped_entries.items():
        lola_entry = utils.get_entry_with_method_from_list("lola", entry)
        lola_time = lola_entry["wallTime"] if lola_entry is not None else "dnf"

        woflan_entry = utils.get_entry_with_method_from_list("woflan", entry)
        woflan_time = woflan_entry["wallTime"] if woflan_entry is not None else "dnf"

        conti_entry = utils.get_entry_with_method_from_list("continuous", entry)
        conti_time = conti_entry["wallTime"] if conti_entry is not None else "dnf"

        data += [[samplename, conti_time, lola_time, woflan_time]]

    data.sort(key=lambda x: int(x[0]))
    size_data.sort(key=lambda x: x[0])
    print("conti")
    print(" ".join(f"({str(entry[0])},{str(float(entry[1])/1000)})" for entry in data))

    print("lola")
    print(" ".join(f"({str(entry[0])},{str(float(entry[2]/1000))})" for entry in data))

    print("woflan")
    print(" ".join(f"({str(entry[0])},{str(float(entry[3]/1000))})" for entry in data))

    print(np.mean([entry[1] for entry in data if entry[1] < 1000]))



        

