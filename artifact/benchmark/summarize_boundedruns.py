import argparse
import plotting_utils as utils
import numpy
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


def print_statistics(original_entries, transformed_entries):
    print("Printing statistics...")

    names_to_entry_pairs = dict()

    for entry in original_entries:
        name = remove_usual_file_endings(entry["sampleName"])
        matching_entries = [trans_entry  for trans_entry in transformed_entries if remove_usual_file_endings(trans_entry["sampleName"]) == name]
        if len(matching_entries) != 1 and len(matching_entries) != 0:
            raise Exception(f"For entry {name}, have {len(matching_entries)} entries that match")

    bounded_before = [entry for entry in original_entries if "error" not in entry and entry["hasBoundedRuns"]]
    unbounded_before = [entry for entry in original_entries if "error" not in entry and not entry["hasBoundedRuns"]]

    bounded_after = [entry for entry in transformed_entries if "error" not in entry and entry["hasBoundedRuns"]]
    unbounded_after = [entry for entry in transformed_entries if "error" not in entry and not entry["hasBoundedRuns"]]


    print("Total before: " + str(len(original_entries)))
    print("Total after: " + str(len(transformed_entries)))
    print("Number of places before: ")
    df = pandas.Series([entry["places"] for entry in original_entries])
    print(df.describe())
    print("Number of places after: ")
    df = pandas.Series([entry["places"] for entry in transformed_entries])
    print(df.describe())
    print("Bounded before: " + str(len(bounded_before)))
    print("Bounded after: " + str(len(bounded_after)))
    print("Unbounded before: " + str(len(unbounded_before)))
    print("Unbounded after: " + str(len(unbounded_after)))


    print("Bounded after, free choice: " + str(len([entry for entry in bounded_after if entry["isFreeChoice"]])))
    print("Unounded after, free choice: " + str(len([entry for entry in unbounded_after if entry["isFreeChoice"]])))




if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--originalfiles', nargs="+", required=True)
    parser.add_argument('-t', '--transformedfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []
    transformed_entries = []

    # read input
    for filepath in args.originalfiles:
        original_entries += utils.read_json_from_file(filepath)

    for filepath in args.transformedfiles:
        transformed_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries, transformed_entries)
