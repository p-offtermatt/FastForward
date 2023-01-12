import argparse
import plotting_utils as utils
import numpy
import pandas
import matplotlib.pyplot
import tabulate


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

    for entry in entries:
        print(entry["sampleName"])

    entries = [entry for entry in entries if entry['isWorkflowNet']]
    print(f"Total: {len(entries)}")

    print("Number of places : ")
    df = pandas.Series([entry["places"] for entry in entries])
    print(df.describe())

    print("Number of transitions : ")
    df = pandas.Series([entry["transitions"] for entry in entries])
    print(df.describe())

    print("Describe time for conti deadlock:")
    df = pandas.Series([entry["timeForContinuousDeadlock"]
                       for entry in entries])
    print(df.describe())

    print("Describe time for int deadlock:")
    df = pandas.Series([entry["timeForIntegerDeadlock"] for entry in entries])
    print(df.describe())

    print("Describe time for fast termination:")
    df = pandas.Series([entry["timeForFastTerminationCheck"]
                       for entry in entries])
    print(df.describe())

    fast = [
        entry for entry in entries if "error" not in entry and entry["hasFastTermination"]]
    slow = [entry for entry in entries if "error" not in entry and not entry["hasFastTermination"]]

    fast_conti_deadlock = [
        entry for entry in fast if entry["hasContinuousDeadlock"]]
    fast_int_deadlock = [
        entry for entry in fast if entry["hasIntegerDeadlock"]]

    slow_conti_deadlock = [
        entry for entry in slow if entry["hasContinuousDeadlock"]]
    slow_int_deadlock = [
        entry for entry in slow if entry["hasIntegerDeadlock"]]

    table = [["", "total", "continuous deadlock", "integer deadlock"],
             ["fast_termination", len(fast), len(fast_conti_deadlock), len(fast_int_deadlock)],
             ["slow_termination", len(slow), len(slow_conti_deadlock), len(slow_int_deadlock)],
             ]

    print(tabulate.tabulate(table))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries)
