import argparse
import plotting_utils as utils
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

def print_statistics(entries):
    print("Printing statistics...")
    print(f"Total entries: {len(entries)}")
    print("Non-Workflow nets:")
    print([entry["sampleName"] for entry in entries if "isWorkflowNet" not in entry])
    print([entry["sampleName"] for entry in entries if "isWorkflowNet" not in entry or not entry["isWorkflowNet"]])
    print(len(entries))
    print("Workflow nets:")
    entries = [entry for entry in entries if "isWorkflowNet" in entry and entry["isWorkflowNet"]]
    print(len(entries))

    print("Number of entries without smallboundproperties:")
    notSmallBoundProps = [entry for entry in entries if "smallBoundProperties" not in entry or entry["smallBoundProperties"] is None]
    print(len(notSmallBoundProps))

    smallBoundProps = [entry for entry in entries if "smallBoundProperties" in entry and entry["smallBoundProperties"] != None]

    
    print("\nSanity checks: Fast Termination iff A_n != -1:")
    print("Entries with fast termination but A_n == -1:")
    print([entry["sampleName"] for entry in smallBoundProps if entry["hasFastTermination"] and entry["smallBoundProperties"]["A_n"] == -1])
    print("Entries without fast termination but A_n != -1:")
    print([entry["sampleName"] for entry in smallBoundProps if not entry["hasFastTermination"] and entry["smallBoundProperties"]["A_n"] != -1])
    print("\n")

    print("Number of fast-termination entries:")
    print(len([entry for entry in smallBoundProps if entry["hasFastTermination"]]))
    print("\n")
    
    PrintAnInfo(smallBoundProps)

def PrintAnInfo(smallBoundProps):
    ans = [(prop["smallBoundProperties"]["A_n"]/prop["transitions"], prop["smallBoundProperties"]["timeForComputingA_n"]) for prop in smallBoundProps]
    ans.sort(key = lambda x: x[0])

    for range in [(-1,0), (0,0.75), (0.75,1), (1, 1.00000000000001), (1.00000000000001, 1.75), (1.75, 1000)]:
        entries_in_range = [an for an in ans if range[0] <= an[0] < range[1]]
        total = len(entries_in_range)
        print(f"[{range[0]}, {range[1]}): {total}")
        df = pandas.Series([entry[1] for entry in entries_in_range])
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
