import argparse
import plotting_utils as utils
import numpy
import pandas
import matplotlib.pyplot


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


def print_statistics(entries):
    print("Printing statistics...")

    transitionMultResults = dict()
    for entry in entries:
        if entry["transitionMultResults"] is None:
            if entry["isWorkflowNet"]:
                print("bad " + entry["sampleName"])
                exit(2)
            else:
                continue
        for mult in entry["transitionMultResults"]:
            mult["originalEntry"] = entry
            transitionMultResults[mult["bound"]] = transitionMultResults.get(
                mult["bound"], []) + [mult]


    print(f"total number: {len(entries)}")

    print("bound -- number that exceeds -- number of that with unbounded runs")
    for key, value in transitionMultResults.items():
        exceeders = [entry for entry in value if entry["boundExceeded"]]
        boundedRuns = [
            entry for entry in exceeders if not entry["originalEntry"]["hasBoundedRuns"]]
        print(f"{key} -- {len(exceeders)} -- {len(boundedRuns)}")
        # print(pandas.Series([entry["timeTaken"]
        #                for entry in value]).describe())

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries)
