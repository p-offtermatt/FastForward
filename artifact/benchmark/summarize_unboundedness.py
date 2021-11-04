import argparse
import plotting_utils as utils
import numpy
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    totalTimes = pandas.Series(numpy.array(
        [entry["wallTime"]/1000 for entry in entries if "error" not in entry]))
    numPlaces = pandas.Series(numpy.array(
        [entry["places"] for entry in entries if "error" not in entry]))
    numTransitions = pandas.Series(numpy.array(
        [entry["transitions"] for entry in entries if "error" not in entry]))
    nonWorkflowNets = pandas.Series(numpy.array([entry for entry in entries if "error" not in entry and
                                                 not entry["isWorkflowNet"]]))
    unboundedTimes = pandas.Series(numpy.array(
        [entry["timeForWFIntegerBoundednessCounterexample"]/1000
         for entry in entries if
         "error" not in entry and
         entry["isWorkflowNet"] == True
         and entry["wfIntegerBoundednessCounterexample"] != "None"]))
    boundedTimes = pandas.Series(numpy.array(
        [entry["timeForWFIntegerBoundednessCounterexample"]/1000 for entry in entries if
         "error" not in entry and
         entry["isWorkflowNet"] == True
         and entry["wfIntegerBoundednessCounterexample"] == "None"]))
    boundedAndSoundTimes = pandas.Series(numpy.array(
        [entry["timeForWFIntegerBoundednessCounterexample"]/1000 for entry in entries if "error" not in entry and entry["wfIntegerBoundednessCounterexample"] == "None"
         and not entry["isContinuousSound"]]))

    print("Non-workflow nets")
    print(len(nonWorkflowNets))
    print("NumPlaces")
    print(numPlaces.describe())
    print("NumPlaces")
    print(numTransitions.describe())
    print("Total times, in seconds")
    print(totalTimes.describe())

    print("Times for unbounded instances, in seconds")
    print(unboundedTimes.describe())

    print("Times for bounded instances, in seconds")
    print(boundedTimes.describe())

    print("Times for bounded and not continuously sound instances, in seconds")
    print(boundedAndSoundTimes.describe())


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
