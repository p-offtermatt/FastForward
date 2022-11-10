import argparse
import plotting_utils as utils
import numpy
import pandas

def print_statistics(entries):
    print("Printing statistics...")
    bounded_runs = [entry["hasBoundedRuns"]for entry in entries if "error" not in entry]
    bounded_runs_true = [entry for entry in bounded_runs if entry]
    bounded_runs_false = [entry for entry in bounded_runs if not entry]

    print("Total: " + str(len(entries)))
    print("Have bounded runs: " + str(len(bounded_runs_true)))
    print("Have unbounded runs: " + str(len(bounded_runs_false)))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    # read input
    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)
    
    print_statistics(json_obj)