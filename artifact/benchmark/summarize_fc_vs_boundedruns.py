import matplotlib.pyplot as plt
import argparse
import plotting_utils as utils
import numpy
import pandas
import matplotlib
matplotlib.use('Agg')


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


def get_first_matching_entry(entry, candidates):
    for other in candidates:
        if remove_usual_file_endings(other["sampleName"]) == remove_usual_file_endings(entry["sampleName"]):
            return other
    print("No matching entry for entry " + entry["sampleName"])


def print_statistics(lola_entries, boundedrun_entries):

    entry_pairs = []
    for lola_entry in lola_entries:
        bounded_entry = get_first_matching_entry(
            lola_entry, boundedrun_entries)
        lola_time = lola_entry["wallTime"]
        entry_pairs.append(
            (lola_time if lola_time != 120000 else 10, bounded_entry["timeForContinuousDeadlock"]))

    print([entry[0] for entry in entry_pairs])

    plt.plot([entry[0] for entry in entry_pairs], [entry[1]
             for entry in entry_pairs], 'ro')
    plt.axis([0, 10, 0, 10])
    plt.xlabel("Lola")
    plt.ylabel("Conti Deadlock")
    plt.plot([0,10], [0,10])
    plt.savefig("lola_vs_bounded_runs")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-l', '--lolafile', required=True)
    parser.add_argument('-b', '--boundedrunfile', required=True)

    args = parser.parse_args()

    lola_entries = []

    # read input
    lola_entries += [entry for entry in utils.read_json_from_file(
        args.lolafile) if entry["methodName"] == "lola"]
    boundedrun_entries = utils.read_json_from_file(args.boundedrunfile)

    print_statistics(lola_entries, boundedrun_entries)
