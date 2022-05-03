import argparse
import os
from pathlib import Path

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
    parser.add_argument('-t', '--target_directory', help = "The directory that the data should be written into.", required = True)
    parser.add_argument('-to', '--timeout', help = "The assumed timeout time of the experiment whose results are processed, in seconds.", required = True, type=int)

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    data = []
    headers = ["N", "continuous", "lola", "woflan"]

    size_data = []

    entries_by_chainnum = dict()

    for filepath in args.datafiles:
        # assumes files are named directorypath/NUMBER_..json
        chain_num = os.path.splitext(filepath)[0].split("/")[-1].split("_")[-2]
        entries = utils.read_json_from_file(filepath)
        entries_by_chainnum[chain_num] = entries_by_chainnum.get(chain_num, []) + entries

    timeouts = dict()
    
    for chain_num, entries in entries_by_chainnum.items():
        for entry in entries:
            if "error" in entry:
                entry["wallTime"] = args.timeout * 1000

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

        timeout_woflans = [entry for entry in woflan_entries if entry["wallTime"] == args.timeout * 1000]
        print(len(timeout_woflans))
        timeout_fraction = len(timeout_woflans) / len(woflan_entries)
        timeouts[chain_num] = timeout_fraction


        lola_times = [entry["wallTime"] for entry in lola_entries]
        continuous_reach_times = [entry["wallTime"] for entry in continuous_reach_entries]
        woflan_times = [entry["wallTime"] for entry in woflan_entries]

        sizes = [entry["numberOfPlaces"] + entry["numberOfTransitions"] for entry in continuous_reach_entries if "numberOfPlaces" in entry and "numberOfTransitions" in entry]
        size_data += [[int(chain_num), sizes]]


        data += [[chain_num, continuous_reach_times, lola_times, woflan_times]]
    data.sort(key=lambda x: int(x[0]))
    size_data.sort(key=lambda x: x[0])

    funcname_to_foldername = {"mean": "means",
    "amax": "maximums", "amin": "minimums"}

    with open(f"{args.target_directory}/timeout.tex", 'w') as file:
            file.write(
                r"\addplot[ultra thick, color=gray, opacity=0.5] coordinates {" + 
                f"(1,{args.timeout}) (401,{args.timeout})" +
                r"};"
            )

    for func in [np.mean, np.min, np.max]:
        foldername = funcname_to_foldername[func.__name__]

        Path(f"{args.target_directory}/{foldername}").mkdir(parents=True, exist_ok=True)
        print("--------------- Writing in folder " + foldername + "-----------------")
        
        print(f"writing {args.target_directory}/{foldername}/continuous.tex")
        with open(f"{args.target_directory}/{foldername}/continuous.tex", 'w') as file:
            file.write(
                (r"\addplot[thick, color=colConti, mark=*, mark size=1.2pt] coordinates {" if foldername == "means" else
                r"\addplot[ultra thin, color=colConti!50!white, mark=*, mark size=1.2pt] coordinates {") + 
                " ".join(f"({str(entry[0])},{str(func(entry[1]) / 1000)})" for entry in data) +
                r"};"
            )

        print(f"writing {args.target_directory}/{foldername}/lola.tex")
        with open(f"{args.target_directory}/{foldername}/lola.tex", 'w') as file:
            file.write(
                (r"\addplot[thick, color=colLola, mark=square*, mark size=1.2pt] coordinates {" if foldername == "means" else
                r"\addplot[ultra thin, color=colLola!50!white, mark=square*, mark size=1.2pt] coordinates {") + 
                " ".join(f"({str(entry[0])},{str(func(entry[2]) / 1000)})" for entry in data) +
                r"};"
            )

        print(f"writing {args.target_directory}/{foldername}/woflan.tex")
        with open(f"{args.target_directory}/{foldername}/woflan.tex", 'w') as file:
            file.write(
                (r"\addplot[thick, color=colWoflan, mark=diamond*, mark size=1.2pt] coordinates {" if foldername == "means" else
                r"\addplot[ultra thin, color=colWoflan!50!white, mark=diamond*, mark size=1.2pt] coordinates {") + 
                " ".join(f"({str(entry[0])},{str(func(entry[3]) / 1000)})" for entry in data) +
                r"};"
            )

        print(f"writing {args.target_directory}/{foldername}/size.tex")
        with open(f"{args.target_directory}/{foldername}/size.tex", 'w') as file:
            file.write(
                (r"\addplot[thick, color=black, mark=x, mark size=1.2pt] coordinates {" if foldername == "means" else
                r"\addplot[ultra thin, color=black!50!white, mark=x, mark size=1.2pt] coordinates {") + 
                " ".join(f"({str(entry[0])},{str(func(entry[1]))})" for entry in data) +
                r"};"
            )

    print(sorted(timeouts.items(), key=lambda x: int(x[0])))

