import argparse
import plotting_utils as utils
import numpy
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    entries = [entry for entry in entries if "error" not in entry or not entry["error"] == "timeout"]
    print(f"Non-timeout-runs: {len(entries)}")
    print("----------------------------------------")

    entries = [entry for entry in entries if "error" not in entry or not entry["error"] == "memout"]
    print(f"Non-memout-runs: {len(entries)}")
    print("----------------------------------------")

    sound_entries = pandas.Series([entry["wallTime"]/1000 for entry in entries if entry["lola_special_commentary"]["analysis"]["result"]])
    print(f"Sound entries:")
    print(sound_entries.describe())
    print("----------------------------------------")

    unsound_entries = pandas.Series([entry["wallTime"]/1000 for entry in entries if not entry["lola_special_commentary"]["analysis"]["result"]])
    print(f"Unsound entries:")
    print(unsound_entries.describe())
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
