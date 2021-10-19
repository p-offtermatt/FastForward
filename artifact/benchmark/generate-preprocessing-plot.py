import csv
import pandas
import sys
import json
from tabulate import tabulate
import argparse
import math
from statistics import mean, median
import plotting_utils as utils

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+",
                        help="datafiles of net statistics")
    parser.add_argument("-op", "--outputfile_places", type=str, required=True)
    parser.add_argument("-ot", "--outputfile_transitions", type=str, required=True)



    args = parser.parse_args()

    collected_data = []

    # read input
    for filepath in args.datafiles:
        json_obj = utils.read_json_from_file(filepath)
        collected_data += json_obj
    
    collected_data = [entry for entry in collected_data if entry.get("error", "") == ""]

    collected_data.sort(key=lambda entry: entry["placesAfterPruning"])
    collected_data.sort(key=lambda entry: entry["places"])


    df = pandas.Series([entry["places"] for entry in collected_data])
    print("\nNumber of places per instance")
    print(df.describe())

    df = pandas.Series([entry["transitions"] for entry in collected_data])
    print("\nNumber of transitions per instance")
    print(df.describe())


    place_buckets = [0] * 11
    transition_buckets = [0] * 11
    for entry in collected_data:
        places_pruned_percent = entry["fractionOfPlacesPruned"] * 100
        place_buckets[round(places_pruned_percent / 10)] += 1

        transitions_pruned_percent = entry["fractionOfTransitionsPruned"] * 100
        transition_buckets[round(transitions_pruned_percent / 10)] += 1

    df = pandas.Series([entry["fractionOfPlacesPruned"] for entry in collected_data])
    print("\nFraction of places pruned:")
    print(df.describe())

    df = pandas.Series([entry["fractionOfTransitionsPruned"] for entry in collected_data])
    print("\nFraction of transitions pruned:")
    print(df.describe())

    place_string = r"\addplot+[ybar interval,mark=no] plot coordinates { "
    place_string += " ".join(["(" + str(bucket*10 - 5) + ", " + str(instances) + ")" for bucket, instances in enumerate(place_buckets)]) + "(105, 0)" # because interval plots skip the last point
    place_string += " };"

    print("\n")
    
    with open(args.outputfile_places, 'w') as place_file:
        place_file.write(place_string)
        print("Wrote place data to " + str(place_file.name))

    transition_string = r"\addplot+[ybar interval,mark=no] plot coordinates { "
    transition_string += " ".join(["(" + str(bucket*10 - 5) + ", " + str(instances) + ")" for bucket, instances in enumerate(transition_buckets)]) + "(105, 0)" # because interval plots skip the last point
    transition_string += " };"

    with open(args.outputfile_transitions, 'w') as transition_file:
        transition_file.write(transition_string)
        print("Wrote transition data to " + str(place_file.name))



