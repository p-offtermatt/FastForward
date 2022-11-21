import argparse
import plotting_utils as utils
import numpy
import pandas
import matplotlib.pyplot

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

    entries = [entry for entry in entries if entry["sampleName"].endswith("_1-check.lola")]

    for entry in entries:
        print(entry["sampleName"])


    bounded = [entry for entry in entries if "error" not in entry and entry["hasBoundedRuns"]]
    unbounded = [entry for entry in entries if "error" not in entry and not entry["hasBoundedRuns"]]


    print("Total : " + str(len(entries)))
    print("Number of places : ")
    df = pandas.Series([entry["places"] for entry in entries])
    print(df.describe())
    print("Bounded : " + str(len(bounded)))
    print("Unbounded : " + str(len(unbounded)))


    print("Bounded , free choice: " + str(len([entry for entry in bounded if entry["isFreeChoice"]])))
    print("Unbounded , free choice: " + str(len([entry for entry in unbounded if entry["isFreeChoice"]])))

    bdAftContDead = [entry for entry in bounded if entry["hasContinuousDeadlock"]]
    bdAftIntDead = [entry for entry in bdAftContDead if entry["hasIntegerDeadlock"]]

    print("-------------- Bounded  -------------------------")
    print("Conti Deadlock: " + str(len(bdAftContDead)))
    print("Int Deadlock: " + str(len(bdAftIntDead)))

    print("Describe time for conti deadlock:")
    df = pandas.Series([entry["timeForContinuousDeadlock"] for entry in entries])
    print(df.describe())

    print("Describe time for int deadlock:")
    df = pandas.Series([entry["timeForIntegerDeadlock"] for entry in entries])
    print(df.describe())

    print("Describe time for conti soundness:")
    df = pandas.Series([entry["timeForContinuousSoundness"] for entry in entries])
    print(df.describe())

    for entry in entries:
        if entry["hasContinuousDeadlock"] == entry["isContinuousSound"] or entry["hasIntegerDeadlock"] == entry["isContinuousSound"]:
            print("Something is wrong!")
            print(entry["sampleName"])






if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries)
