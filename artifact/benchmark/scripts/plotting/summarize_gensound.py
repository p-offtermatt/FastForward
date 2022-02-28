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
    parser.add_argument('-t', '--target_directory', help = "The directory that the data should be written into.", required = True)
    parser.add_argument('-to', '--timeout', help = "The assumed timeout time of the experiment whose results are processed, in seconds.", required = True, type=int)


    args = parser.parse_args()

    timeout_in_ms = args.timeout * 1000
    print(timeout_in_ms)

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
            entry["wallTime"] = timeout_in_ms

    grouped_entries = utils.group_by_name(entries)

    data = []

    for samplename, entry in grouped_entries.items():
        lola_entry = utils.get_entry_with_method_from_list("lola", entry)
        lola_time = lola_entry["wallTime"] if lola_entry is not None else timeout_in_ms

        woflan_entry = utils.get_entry_with_method_from_list("woflan", entry)
        woflan_time = woflan_entry["wallTime"] if woflan_entry is not None else timeout_in_ms

        conti_entry = utils.get_entry_with_method_from_list("continuous", entry)
        conti_time = conti_entry["wallTime"] if conti_entry is not None else timeout_in_ms

        data += [[samplename.split("-")[0], conti_time, lola_time, woflan_time]]

    size_data.sort(key=lambda x: x[0])
    print(f"writing {args.target_directory}/continuous.tex")
    with open(args.target_directory + "/continuous.tex", 'w') as file:
        file.write(
            r"\addplot[thick, color=colConti, mark=*, mark size=1.2pt] coordinates {" + 
            " ".join(f"({str(entry[0])},{str(float(entry[1])/1000)})" for entry in data) +
            r"};"
        )

    print(f"writing {args.target_directory}/lola.tex")
    with open(args.target_directory + "/lola.tex", 'w') as file:
        file.write(
            r"\addplot[thick, color=colLola, mark=square*, mark size=1.2pt] coordinates {" + 
            " ".join(f"({str(entry[0])},{str(float(entry[2])/1000)})" for entry in data) +
            r"};"
        )

    print(f"writing {args.target_directory}/woflan.tex")
    with open(args.target_directory + "/woflan.tex", 'w') as file:
        file.write(
            r"\addplot[thick, color=colWoflan, mark=diamond*, mark size=1.2pt] coordinates {" + 
            " ".join(f"({str(entry[0])},{str(float(entry[3])/1000)})" for entry in data) +
            r"};"
        )

    print(f"writing {args.target_directory}/timeout.tex")
    with open(args.target_directory + "/timeout.tex", 'w') as file:
        file.write(
            r"\addplot[ultra thick, color=gray, opacity=0.5] coordinates {" + 
            f"(1,{args.timeout}) (41,{args.timeout})"
            r"};"
        )



        

