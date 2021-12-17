import argparse
import plotting_utils as utils
import numpy
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    print("Number of entries")
    print(len(entries))
    entries = [entry for entry in entries if "error" not in entry]
    places = pandas.Series(numpy.array(
        [entry["numberOfPlaces"] for entry in entries if "error" not in entry]))
    print("Places...")
    print(places.describe())

    transitions = pandas.Series(numpy.array(
        [entry["numberOfTransitions"] for entry in entries if "error" not in entry]))
    print("Transitions...")
    print(transitions.describe())

    totalTimes = pandas.Series(numpy.array(
        [entry["timeInQuery"]/1000 for entry in entries if "error" not in entry]))
    soundTimes = pandas.Series(numpy.array(
        [entry["timeInQuery"]/1000 for entry in entries if "error" not in entry and entry["allTransitionsExpressible"]]))
    unsoundTimes = pandas.Series(numpy.array(
        [entry["timeInQuery"]/1000 for entry in entries if "error" not in entry and not entry["allTransitionsExpressible"]]))
    print("Total times, in seconds")
    print(totalTimes.describe())

    print("Times for sound instances, in seconds")
    print(soundTimes.describe())

    print("Times for unsound instances, in seconds")
    print(unsoundTimes.describe())

    entries.sort(key=lambda entry: entry["timeInQuery"])
    print([(entry["file"], entry["timeInQuery"]) for entry in entries[-10:]])


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
