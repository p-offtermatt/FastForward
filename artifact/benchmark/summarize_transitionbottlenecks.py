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

    for entry in entries:
        if entry["transitionBottleneckNums"] is None:
            if entry["isWorkflowNet"]:
                print("bad " + entry["sampleName"])
                exit(2)
            else:
                continue
    entries = [entry for entry in entries if entry["transitionBottleneckNums"]]

    print("Number of transitions : ")
    df = pandas.Series([entry["transitions"] for entry in entries])
    print(df.describe())

    print(f"total number: {len(entries)}")

    print("number - largest bottleneck - smallest bottleneck - mean bottleneck")
    transitionBottlenecks = [
        [num for (_, num) in entry["transitionBottleneckNums"].items()] for entry in entries
    ]

    transitionBottleneckStrings = [f"{max(entry)} : {min(entry)} : {sum(entry) / len(entry)}" for entry in transitionBottlenecks]

    transitionBottleneckSet = [
        f"{transitionBottleneckStrings.count(elem)} : " + elem for elem in set(transitionBottleneckStrings)
    ]
    
    for entry in transitionBottleneckSet:
        print(entry)

    special_entries = []

    for entry in entries:
        bottlenecks = entry["transitionBottleneckNums"]
        nums = [num for (transition, num) in bottlenecks.items()]
        if [num for num in nums if num not in {-1.0, 1.0}]:
            special_entries.append(nums)

    # for entry in transitionBottlenecks:
    #     print(entry)

    print("-----------\nNumbers of bottlenecks:")
    df = pandas.Series([len([num for num in entry if num == max(entry)]) for entry in transitionBottlenecks if max(entry) not in {-1.0}])
    print(df.describe())
    
        

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries)
