import argparse
import plotting_utils as utils
import numpy
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    error_free_entries = [entry for entry in entries if "error" not in entry]
    print(f"Error-free entries: {len(error_free_entries)}")
    print("----------------------------------------")


    workflow_entries = [
        entry for entry in error_free_entries if entry["isWorkflowNet"]]
    print(f"Workflow nets: {len(workflow_entries)}")
    print("----------------------------------------")

    integer_bounded_entries = [entry for entry in workflow_entries if entry["wfIntegerBoundednessCounterexample"] == "None"]
    print(f"Integer-bounded entries: {len(integer_bounded_entries)}")
    print("----------------------------------------")

    integer_sound_nets = pandas.Series([entry["timeForIntegerSoundness"]/1000 for entry in integer_bounded_entries if entry["isIntegerSound"] == True])
    print(f"Times for integer-sound nets:")
    print(integer_sound_nets.describe())
    print("----------------------------------------")

    integer_unsound_nets = pandas.Series([entry["timeForIntegerSoundness"]/1000 for entry in integer_bounded_entries if entry["isIntegerSound"] == False])
    print(f"Times for integer-unsound nets:")
    print(integer_unsound_nets.describe())
    print("----------------------------------------")



    # totalTimes = pandas.Series(numpy.array([entry["timeInQuery"]/1000 for entry in entries if "error" not in entry]))
    # soundTimes = pandas.Series(numpy.array([entry["timeInQuery"]/1000 for entry in entries if "error" not in entry and entry["isContinuousSound"]]))
    # unsoundTimes = pandas.Series(numpy.array([entry["timeInQuery"]/1000 for entry in entries if "error" not in entry and not entry["isContinuousSound"]]))
    # print("Total times, in seconds")
    # print(totalTimes.describe())

    # print("Times for sound instances, in seconds")
    # print(soundTimes.describe())

    # print("Times for unsound instances, in seconds")
    # print(unsoundTimes.describe())


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
