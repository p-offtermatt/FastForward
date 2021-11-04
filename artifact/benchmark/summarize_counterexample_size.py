import argparse
import plotting_utils as utils
import numpy
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    counterexample_sizes = pandas.Series(numpy.array(
        [entry["wfIntegerBoundednessCounterexampleImageSize"]
         for entry in entries if
         "error" not in entry and
         entry["isWorkflowNet"] == True
         and entry["wfIntegerBoundednessCounterexample"] != "None"]))
    counterexample_support_sizes = pandas.Series(numpy.array(
        [entry["wfIntegerBoundednessCounterexampleSupportSize"]
         for entry in entries if
         "error" not in entry and
         entry["isWorkflowNet"] == True
         and entry["wfIntegerBoundednessCounterexample"] != "None"]))

    print("Image Sizes")
    print(counterexample_sizes.describe())

    print("Support sizes")
    print(counterexample_support_sizes.describe())


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
