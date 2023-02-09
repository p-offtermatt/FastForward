import argparse
from plotting_utils import read_json_from_file
import pandas
import tabulate

spacer = "==============================="

def print_statistics(entries):
    print("Printing statistics...")

    print(f"{len(entries)} entries.")

    print(spacer)

    timeouts = [entry for entry in entries if "error" in entry and entry["error"] == "timeout"]
    print(f"{len(timeouts)} timeouts on instances:")
    for entry in timeouts:
        print(entry["sampleName"])

    print(spacer)
    
    error_free = [entry for entry in entries if "error" not in entry]

    print(f"{len(error_free)} error free instances:")
    for entry in error_free:
        print(entry["sampleName"])

    print(spacer)

    continuous_unsound = [entry for entry in error_free if not entry["isContinuousSound"]]

    print(f"{len(continuous_unsound)} entries are not continuously sound")

    print(spacer)

    fastTerminating = [entry for entry in error_free if entry["hasFastTermination"]]

    print(f"{len(fastTerminating)} entries are terminating")

    print(spacer)

    print("Values for the linear constant a_n:")
    for entry in error_free:
        print(f"{entry['sampleName']} has a_n = {entry['smallBoundProperties']['A_n']}")






if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += read_json_from_file(filepath)

    print_statistics(original_entries)
