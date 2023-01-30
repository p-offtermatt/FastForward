import argparse
import plotting_utils as utils
import pandas


def remove_usual_file_endings(input: str):
    while True:
        if input.endswith(".xml"):
            input = input[:-(len(".xml"))]
        elif input.endswith(".lola"):
            input = input[:-(len(".lola"))]
        elif input.endswith(".tpn"):
            input = input[:-(len(".tpn"))]
        else:
            return input


def find_matching_entry(entry, entries):
    matches = [other for other in entries if other["sampleName"]
               == entry["sampleName"]]
    if len(matches) > 1:
        raise Exception(
            f"Multiple matches for entry {entry['sampleName']} found!")
    if len(matches) == 0:
        return None

    return matches[0]


def print_statistics(original, reduced):
    print("Printing statistics...")

    print([entry["sampleName"] for entry in original if "smallBoundProperties" in entry and entry["smallBoundProperties"]
          is not None and "minTime" not in entry["smallBoundProperties"]])

    reducedProps = [
        entry for entry in reduced if "smallBoundProperties" in entry and entry["smallBoundProperties"] != None and entry["hasFastTermination"]]
    originalProps = [entry for entry in original if "smallBoundProperties" in entry and entry["smallBoundProperties"]
                     is not None and entry["hasFastTermination"] and find_matching_entry(entry, reducedProps) is not None]

    increased_maxtime = [entry for entry in originalProps if entry["smallBoundProperties"]["maxTime"] != "timeout" and find_matching_entry(
        entry, reducedProps)["smallBoundProperties"]["maxTime"] > entry["smallBoundProperties"]["maxTime"]]
    increased_an = [entry for entry in originalProps if entry["smallBoundProperties"]["A_n"] != "timeout" and find_matching_entry(
        entry, reducedProps)["smallBoundProperties"]["A_n"] > entry["smallBoundProperties"]["A_n"]]
    increased_mintime = [entry for entry in originalProps if entry["smallBoundProperties"]["minTime"] != "timeout" and find_matching_entry(
        entry, reducedProps)["smallBoundProperties"]["minTime"] > entry["smallBoundProperties"]["minTime"]]

    print(f"Increased A_n:")
    for entry in increased_an:
        print(entry["sampleName"])
    print(f"Increased MaxTime: {len(increased_maxtime)}")
    for entry in increased_maxtime:
        print(entry["sampleName"])
    print(f"Increased MinTime: {len(increased_mintime)}")
    for entry in increased_mintime:
        print(entry["sampleName"], entry["smallBoundProperties"]["minTime"], find_matching_entry(
            entry, reducedProps)["smallBoundProperties"]["minTime"])


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--statsonoriginal', nargs="+", required=True)
    parser.add_argument('-r', '--statsonreduced', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.statsonoriginal:
        original_entries += utils.read_json_from_file(filepath)

    reduced_entries = []

    # read input
    for filepath in args.statsonreduced:
        reduced_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries, reduced_entries)
